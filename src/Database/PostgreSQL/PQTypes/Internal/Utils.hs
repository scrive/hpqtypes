{-# LANGUAGE RecordWildCards #-}
module Database.PostgreSQL.PQTypes.Internal.Utils (
    cStringLenToBytea
  , byteaToCStringLen
  , bsToCString
  , hpqTypesError
  , unexpectedNULL
  , verifyPQTRes
  , withPGparam
  ) where

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

-- | Throw 'HPQTypesError exception.
hpqTypesError :: String -> IO a
hpqTypesError = E.throwIO . HPQTypesError

-- | Throw 'unexpected NULL' exception.
unexpectedNULL :: IO a
unexpectedNULL = hpqTypesError "unexpected NULL"

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
