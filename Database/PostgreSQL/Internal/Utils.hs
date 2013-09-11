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
import qualified Control.Exception as E

import Database.PostgreSQL.Internal.Error

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

verifyPQTRes :: String -> CInt -> IO ()
verifyPQTRes ctx 0 = throwLibPQTypesError ctx
verifyPQTRes _ _ = return ()
