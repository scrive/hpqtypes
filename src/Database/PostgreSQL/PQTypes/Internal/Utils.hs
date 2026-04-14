module Database.PostgreSQL.PQTypes.Internal.Utils
  ( MkConstraint
  , mread
  , safePeekCString
  , safePeekCString'
  , cStringLenToBytea
  , byteaToCStringLen
  , textToCString
  , numericVarToInteger
  , withIntegerAsNumericVar
  , verifyPQTRes
  , withPGparam
  , throwLibPQError
  , throwLibPQTypesError
  , rethrowWithArrayError
  , hpqTypesError
  , unexpectedNULL
  ) where

import Control.Exception qualified as E
import Control.Monad
import Data.ByteString.Unsafe
import Data.Kind (Type)
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Vector.Storable qualified as V
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import GHC.Exts
import GHC.Stack

import Database.PostgreSQL.PQTypes.Internal.C.Interface
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Error

type family
  MkConstraint
    (m :: Type -> Type)
    (cs :: [(Type -> Type) -> Constraint])
    :: Constraint
  where
  MkConstraint m '[] = ()
  MkConstraint m (c ': cs) = (c m, MkConstraint m cs)

-- Safely read value.
mread :: Read a => String -> Maybe a
mread s = do
  [(a, "")] <- Just (reads s)
  Just a

-- | Safely peek C string.
safePeekCString :: CString -> IO (Maybe String)
safePeekCString cs
  | cs == nullPtr = pure Nothing
  | otherwise = Just <$> peekCString cs

-- | Safely peek C string and return "" if NULL.
safePeekCString' :: CString -> IO String
safePeekCString' cs = fromMaybe "" <$> safePeekCString cs

-- | Convert C string to 'PGbytea'.
cStringLenToBytea :: CStringLen -> PGbytea
cStringLenToBytea (cs, len) =
  PGbytea
    { pgByteaLen = fromIntegral len
    , pgByteaData = cs
    }

-- | Convert 'PGbytea' to C string.
byteaToCStringLen :: PGbytea -> CStringLen
byteaToCStringLen PGbytea {..} = (pgByteaData, fromIntegral pgByteaLen)

-- | Convert 'Text' to UTF-8 encoded C string wrapped by foreign pointer.
textToCString :: T.Text -> IO (ForeignPtr CChar)
textToCString bs = unsafeUseAsCStringLen (T.encodeUtf8 bs) $ \(cs, len) -> do
  fptr <- mallocForeignPtrBytes (len + 1)
  withForeignPtr fptr $ \ptr -> do
    copyBytes ptr cs len
    pokeByteOff ptr len (0 :: CChar)
  pure fptr

----------------------------------------

-- Note: these can be generalized to convert from/to Scientific and support
-- arbitrary floating point precision (relevant code can be borrowed from the
-- postgresql-binary package), but while deserialization is easy either way,
-- serialization is significantly more annoying, so let's leave this until it's
-- actually needed.

numericVarToInteger :: NumericVar -> IO Integer
numericVarToInteger NumericVar {..}
  | numVarDscale /= 0 = hpqTypesError "not an integer"
  | numVarSign == c_NUMERIC_NAN = hpqTypesError "not a number"
  | numVarSign == c_NUMERIC_POS = mkInteger 0 numVarDigits numVarNdigits
  | numVarSign == c_NUMERIC_NEG = negate <$> mkInteger 0 numVarDigits numVarNdigits
  | otherwise = hpqTypesError $ "unexpected sign: " ++ show numVarSign
  where
    mkInteger :: Integer -> Ptr CShort -> CShort -> IO Integer
    mkInteger acc ptr = \case
      0 -> pure acc
      n -> do
        v <- ntohs <$> peek ptr
        when (v < 0 || v > 9999) $ do
          hpqTypesError $ "invalid digit: " ++ show v
        mkInteger (acc * 10000 + fromIntegral v) (ptr `plusPtr` 2) (n - 1)

withIntegerAsNumericVar :: Integer -> (NumericVar -> IO r) -> IO r
withIntegerAsNumericVar n k = V.unsafeWith digits $ \digitsPtr -> do
  k $
    NumericVar
      { numVarNdigits = digitsLen
      , numVarWeight = max 0 (digitsLen - 1)
      , numVarSign = if n < 0 then c_NUMERIC_NEG else c_NUMERIC_POS
      , numVarDscale = 0
      , numVarDigits = digitsPtr
      }
  where
    digitsLen :: CShort
    digitsLen = fromIntegral $ V.length digits

    digits :: V.Vector CShort
    digits = V.reverse . (`V.unfoldr` abs n) $ \case
      0 -> Nothing
      x -> case x `quotRem` 10000 of
        (d, m) -> Just (htons $ fromIntegral m, d)

foreign import ccall unsafe "htons" htons :: CShort -> CShort
foreign import ccall unsafe "ntohs" ntohs :: CShort -> CShort

----------------------------------------

-- | Check return value of a function from libpqtypes
-- and if it indicates an error, throw appropriate exception.
verifyPQTRes :: HasCallStack => Ptr PGerror -> String -> CInt -> IO ()
verifyPQTRes err ctx 0 = throwLibPQTypesError err ctx
verifyPQTRes _ _ _ = pure ()

-- 'alloca'-like function for managing usage of 'PGparam' object.
withPGparam :: HasCallStack => Ptr PGconn -> (Ptr PGparam -> IO r) -> IO r
withPGparam conn = E.bracket create c_PQparamClear
  where
    create = alloca $ \err -> do
      param <- c_PQparamCreate conn err
      when (param == nullPtr) $
        throwLibPQTypesError err "withPGparam.create"
      pure param

----------------------------------------

-- | Throw libpq specific error.
throwLibPQError :: HasCallStack => Ptr PGconn -> String -> IO a
throwLibPQError conn ctx = do
  msg <- safePeekCString' =<< c_PQerrorMessage conn
  E.throwIO . LibPQError $
    if null ctx then msg else ctx ++ ": " ++ msg

-- | Throw libpqtypes specific error.
throwLibPQTypesError :: HasCallStack => Ptr PGerror -> String -> IO a
throwLibPQTypesError err ctx = do
  msg <- pgErrorMsg <$> peek err
  E.throwIO . LibPQError $
    if null ctx then msg else ctx ++ ": " ++ msg

-- | Rethrow supplied exception enriched with array index.
rethrowWithArrayError :: HasCallStack => CInt -> E.SomeException -> IO a
rethrowWithArrayError i (E.SomeException e) =
  E.throwIO
    ArrayItemError
      { arrItemIndex = fromIntegral i + 1
      , arrItemError = e
      }

-- | Throw 'HPQTypesError exception.
hpqTypesError :: HasCallStack => String -> IO a
hpqTypesError = E.throwIO . HPQTypesError

-- | Throw 'unexpected NULL' exception.
unexpectedNULL :: HasCallStack => IO a
unexpectedNULL = hpqTypesError "unexpected NULL"
