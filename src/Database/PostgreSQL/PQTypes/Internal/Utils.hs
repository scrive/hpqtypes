{-# LANGUAGE DataKinds #-}
module Database.PostgreSQL.PQTypes.Internal.Utils (
    MkConstraint
  , mread
  , safePeekCString
  , safePeekCString'
  , cStringLenToBytea
  , byteaToCStringLen
  , textToCString
  , verifyPQTRes
  , withPGparam
  , throwLibPQError
  , throwLibPQTypesError
  , rethrowWithArrayError
  , hpqTypesError
  , unexpectedNULL
  ) where

import Control.Applicative
import Control.Monad
import Data.ByteString.Unsafe
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import GHC.Exts
import Prelude
import qualified Control.Exception as E
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Database.PostgreSQL.PQTypes.Internal.C.Interface
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Error

type family MkConstraint (m :: * -> *) (cs :: [(* -> *) -> Constraint]) :: Constraint where
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
  | cs == nullPtr = return Nothing
  | otherwise     = Just <$> peekCString cs

-- | Safely peek C string and return "" if NULL.
safePeekCString' :: CString -> IO String
safePeekCString' cs = maybe "" id <$> safePeekCString cs

-- | Convert C string to 'PGbytea'.
cStringLenToBytea :: CStringLen -> PGbytea
cStringLenToBytea (cs, len) = PGbytea {
  pgByteaLen = fromIntegral len
, pgByteaData = cs
}

-- | Convert 'PGbytea' to C string.
byteaToCStringLen :: PGbytea -> CStringLen
byteaToCStringLen PGbytea{..} = (pgByteaData, fromIntegral pgByteaLen)

-- | Convert 'Text' to UTF-8 encoded C string wrapped by foreign pointer.
textToCString :: T.Text -> IO (ForeignPtr CChar)
textToCString bs = unsafeUseAsCStringLen (T.encodeUtf8 bs) $ \(cs, len) -> do
  fptr <- mallocForeignPtrBytes (len + 1)
  withForeignPtr fptr $ \ptr -> do
    copyBytes ptr cs len
    pokeByteOff ptr len (0::CChar)
  return fptr

-- | Check return value of a function from libpqtypes
-- and if it indicates an error, throw appropriate exception.
verifyPQTRes :: Ptr PGerror -> String -> CInt -> IO ()
verifyPQTRes err ctx 0 = throwLibPQTypesError err ctx
verifyPQTRes   _   _ _ = return ()

-- 'alloca'-like function for managing usage of 'PGparam' object.
withPGparam :: Ptr PGconn -> (Ptr PGparam -> IO r) -> IO r
withPGparam conn = E.bracket create c_PQparamClear
  where
    create = alloca $ \err -> do
      param <- c_PQparamCreate conn err
      when (param == nullPtr) $
        throwLibPQTypesError err "withPGparam.create"
      return param

----------------------------------------

-- | Throw libpq specific error.
throwLibPQError :: Ptr PGconn -> String -> IO a
throwLibPQError conn ctx = do
  msg <- safePeekCString' =<< c_PQerrorMessage conn
  E.throwIO . LibPQError
    $ if null ctx then msg else ctx ++ ": " ++ msg

-- | Throw libpqtypes specific error.
throwLibPQTypesError :: Ptr PGerror -> String -> IO a
throwLibPQTypesError err ctx = do
  msg <- pgErrorMsg <$> peek err
  E.throwIO . LibPQError
    $ if null ctx then msg else ctx ++ ": " ++ msg

-- | Rethrow supplied exception enriched with array index.
rethrowWithArrayError :: CInt -> E.SomeException -> IO a
rethrowWithArrayError i (E.SomeException e) =
  E.throwIO ArrayItemError {
    arrItemIndex = fromIntegral i + 1
  , arrItemError = e
  }

-- | Throw 'HPQTypesError exception.
hpqTypesError :: String -> IO a
hpqTypesError = E.throwIO . HPQTypesError

-- | Throw 'unexpected NULL' exception.
unexpectedNULL :: IO a
unexpectedNULL = hpqTypesError "unexpected NULL"
