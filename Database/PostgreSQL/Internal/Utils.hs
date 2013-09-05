{-# OPTIONS_GHC -Wall #-}
module Database.PostgreSQL.Internal.Utils where

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe
import Data.List
import Data.Monoid
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Foreign.Storable

import Database.PostgreSQL.Internal.Exception
import Database.PostgreSQL.Internal.SQL

mintercalate :: Monoid m => m -> [m] -> m
mintercalate s = mconcat . intersperse s

bsToCString :: ByteString -> IO (ForeignPtr CChar)
bsToCString bs = unsafeUseAsCStringLen bs $ \(cs, len) -> do
  fptr <- mallocForeignPtrBytes (len + 1)
  withForeignPtr fptr $ \ptr -> do
    copyBytes ptr cs len
    pokeByteOff ptr len (0::CChar)
  return fptr

verifyGetPut :: SQL -> CInt -> IO ()
verifyGetPut ctx 0 = throwLibPQTypesError ctx
verifyGetPut _   _ = return ()
