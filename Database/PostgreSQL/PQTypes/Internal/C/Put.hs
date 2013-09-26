{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Database.PostgreSQL.PQTypes.Internal.C.Put where

import Foreign.C
import Foreign.Ptr
import qualified Data.ByteString.Char8 as BS

import Database.PostgreSQL.PQTypes.Internal.C.Types

foreign import ccall unsafe "PQputf"
  c_PQputf0 :: Ptr PGparam -> Ptr PGerror -> CString -> IO CInt

foreign import ccall unsafe "PQputf"
  c_PQputfCChar :: Ptr PGparam -> Ptr PGerror -> CString -> CChar -> IO CInt

foreign import ccall unsafe "PQputf"
  c_PQputfCShort :: Ptr PGparam -> Ptr PGerror -> CString -> CShort -> IO CInt

foreign import ccall unsafe "PQputf"
  c_PQputfCInt :: Ptr PGparam -> Ptr PGerror -> CString -> CInt -> IO CInt

foreign import ccall unsafe "PQputf"
  c_PQputfCLLong :: Ptr PGparam -> Ptr PGerror -> CString -> CLLong -> IO CInt

foreign import ccall unsafe "PQputf"
  c_PQputfCFloat :: Ptr PGparam -> Ptr PGerror -> CString -> CFloat -> IO CInt

foreign import ccall unsafe "PQputf"
  c_PQputfCDouble :: Ptr PGparam -> Ptr PGerror -> CString -> CDouble -> IO CInt

foreign import ccall unsafe "PQputf"
  c_PQputfPtr :: Ptr PGparam -> Ptr PGerror -> CString -> Ptr t -> IO CInt

----------------------------------------

c_PQPutfMaybe :: PQPut base => Ptr PGparam -> Ptr PGerror -> CString -> Maybe base -> IO CInt
c_PQPutfMaybe param err _ Nothing = BS.useAsCString (BS.pack "%null") (c_PQputf0 param err)
c_PQPutfMaybe param err fmt (Just base) = c_PQPutf param err fmt base

----------------------------------------

class PQPut base where
  c_PQPutf :: Ptr PGparam -> Ptr PGerror -> CString -> base -> IO CInt

instance PQPut CChar where
  c_PQPutf = c_PQputfCChar

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
