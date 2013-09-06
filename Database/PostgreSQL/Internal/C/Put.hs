{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Database.PostgreSQL.Internal.C.Put where

import Data.ByteString.Unsafe
import Foreign.C
import Foreign.Ptr
import qualified Data.ByteString.Char8 as BS

import Database.PostgreSQL.Internal.C.Types

foreign import ccall unsafe "PQputf"
  c_PQputf0 :: Ptr PGparam -> CString -> IO CInt

foreign import ccall unsafe "PQputf"
  c_PQputfCShort :: Ptr PGparam -> CString -> CShort -> IO CInt

foreign import ccall unsafe "PQputf"
  c_PQputfCInt :: Ptr PGparam -> CString -> CInt -> IO CInt

foreign import ccall unsafe "PQputf"
  c_PQputfCLLong :: Ptr PGparam -> CString -> CLLong -> IO CInt

foreign import ccall unsafe "PQputf"
  c_PQputfCFloat :: Ptr PGparam -> CString -> CFloat -> IO CInt

foreign import ccall unsafe "PQputf"
  c_PQputfCDouble :: Ptr PGparam -> CString -> CDouble -> IO CInt

foreign import ccall unsafe "PQputf"
  c_PQputfPtr :: Ptr PGparam -> CString -> Ptr t -> IO CInt

----------------------------------------

c_PQPutfNULL :: Ptr PGparam -> IO CInt
c_PQPutfNULL = unsafeUseAsCString (BS.pack "%null\0") . c_PQputf0

----------------------------------------

class PQPut base where
  c_PQPutf :: Ptr PGparam -> CString -> base -> IO CInt

instance PQPut CShort where
  c_PQPutf = c_PQputfCShort

instance PQPut CInt where
  c_PQPutf = c_PQputfCInt

instance PQPut CLLong where
  c_PQPutf = c_PQputfCLLong

instance PQPut CFloat where
  c_PQPutf = c_PQputfCFloat

instance PQPut CDouble where
  c_PQPutf = c_PQputfCDouble

instance PQPut (Ptr t) where
  c_PQPutf = c_PQputfPtr
