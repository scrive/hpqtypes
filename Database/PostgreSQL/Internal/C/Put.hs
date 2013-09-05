{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
module Database.PostgreSQL.Internal.C.Put where

import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Unsafe
import Foreign.C
import Foreign.Ptr

import Database.PostgreSQL.Internal.C.Types

foreign import ccall unsafe "PQputf"
  c_PQputf0 :: Ptr PGparam -> CString -> IO CInt

foreign import ccall unsafe "PQputf"
  c_PQputfCInt :: Ptr PGparam -> CString -> CInt -> IO CInt

foreign import ccall unsafe "PQputf"
  c_PQputfPtr :: Ptr PGparam -> CString -> Ptr t -> IO CInt

c_PQPutfNULL :: Ptr PGparam -> IO CInt
c_PQPutfNULL = unsafeUseAsCString "%null\0" . c_PQputf0

class PQPut base where
  c_PQPutf :: Ptr PGparam -> CString -> base -> IO CInt

instance PQPut CInt where
  c_PQPutf = c_PQputfCInt

instance PQPut (Ptr t) where
  c_PQPutf = c_PQputfPtr
