{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Database.PostgreSQL.Internal.C.Put where

import Foreign.C
import Foreign.Ptr

import Database.PostgreSQL.Internal.C.Types

foreign import ccall unsafe "PQputf"
  c_PQputfCInt :: Ptr PGparam -> CString -> CInt -> IO CInt

foreign import ccall unsafe "PQputf"
  c_PQputfPtr :: Ptr PGparam -> CString -> Ptr t -> IO CInt

class PutSQL base where
  pqPut :: Ptr PGparam -> CString -> base -> IO CInt

instance PutSQL CInt where
  pqPut = c_PQputfCInt

instance PutSQL (Ptr t) where
  pqPut = c_PQputfPtr
