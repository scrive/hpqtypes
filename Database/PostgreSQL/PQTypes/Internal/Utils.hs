{-# OPTIONS_GHC -Wall #-}
module Database.PostgreSQL.PQTypes.Internal.Utils (
    mintercalate
  , bsToCString
  , unexpectedNULL
  , verifyPQTRes
  , withPGparam
  ) where

import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe
import Data.List
import Data.Monoid
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

mintercalate :: Monoid m => m -> [m] -> m
mintercalate s = mconcat . intersperse s

bsToCString :: ByteString -> IO (ForeignPtr CChar)
bsToCString bs = unsafeUseAsCStringLen bs $ \(cs, len) -> do
  fptr <- mallocForeignPtrBytes (len + 1)
  withForeignPtr fptr $ \ptr -> do
    copyBytes ptr cs len
    pokeByteOff ptr len (0::CChar)
  return fptr

unexpectedNULL :: IO a
unexpectedNULL = E.throwIO . InternalError $ "unexpected NULL"

verifyPQTRes :: Ptr PGerror -> String -> CInt -> IO ()
verifyPQTRes err ctx 0 = throwLibPQTypesError err ctx
verifyPQTRes   _   _ _ = return ()

withPGparam :: Ptr PGconn -> (Ptr PGparam -> IO r) -> IO r
withPGparam conn = E.bracket create c_PQparamClear
  where
    create = alloca $ \err -> do
      param <- c_PQparamCreate conn err
      when (param == nullPtr) $
        throwLibPQTypesError err "withPGparam.create"
      return param
