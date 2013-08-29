{-# OPTIONS_GHC -Wall #-}
module Database.PostgreSQL.Internal.Utils where

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Foreign.Storable

import Database.PostgreSQL.Internal.C.Interface

bsToCString :: ByteString -> IO (ForeignPtr CChar)
bsToCString bs = unsafeUseAsCStringLen bs $ \(cs, len) -> do
  fptr <- mallocForeignPtrBytes (len + 1)
  withForeignPtr fptr $ \ptr -> do
    copyBytes ptr cs len
    pokeByteOff ptr len (0::CChar)
  return fptr

successCheck :: CInt -> IO ()
successCheck 0 = c_PQgeterror >>= peekCString >>= error
successCheck _ = return ()
