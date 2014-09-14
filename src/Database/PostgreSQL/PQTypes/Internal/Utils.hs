{-# LANGUAGE RecordWildCards #-}
module Database.PostgreSQL.PQTypes.Internal.Utils (
    mread
  , safePeekCString
  , safePeekCString'
  , cStringLenToBytea
  , byteaToCStringLen
  , bsToCString
  , verifyPQTRes
  , withPGparam
  , throwQueryError
  , throwLibPQError
  , throwLibPQTypesError
  , rethrowWithArrayError
  , hpqTypesError
  , unexpectedNULL
  ) where

import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import qualified Control.Exception as E

import Database.PostgreSQL.PQTypes.Internal.C.Interface
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.Internal.Error.Code

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

-- | Convert 'ByteString' to C string wrapped by foreign pointer.
bsToCString :: ByteString -> IO (ForeignPtr CChar)
bsToCString bs = unsafeUseAsCStringLen bs $ \(cs, len) -> do
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

-- | Throw query error.
throwQueryError :: Ptr PGconn -> Ptr PGresult -> IO a
throwQueryError conn res = if res == nullPtr
  then E.throwIO . QueryError =<< safePeekCString' =<< c_PQerrorMessage conn
  else E.throwIO =<< DetailedQueryError
    <$> field c_PG_DIAG_SEVERITY
    <*> (stringToErrorCode <$> field c_PG_DIAG_SQLSTATE)
    <*> field c_PG_DIAG_MESSAGE_PRIMARY
    <*> mfield c_PG_DIAG_MESSAGE_DETAIL
    <*> mfield c_PG_DIAG_MESSAGE_HINT
    <*> ((mread =<<) <$> mfield c_PG_DIAG_STATEMENT_POSITION)
    <*> ((mread =<<) <$> mfield c_PG_DIAG_INTERNAL_POSITION)
    <*> mfield c_PG_DIAG_INTERNAL_QUERY
    <*> mfield c_PG_DIAG_CONTEXT
    <*> mfield c_PG_DIAG_SOURCE_FILE
    <*> ((mread =<<) <$> mfield c_PG_DIAG_SOURCE_LINE)
    <*> mfield c_PG_DIAG_SOURCE_FUNCTION
  where
    field f = maybe "" id <$> mfield f
    mfield f = safePeekCString =<< c_PQresultErrorField res f

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
