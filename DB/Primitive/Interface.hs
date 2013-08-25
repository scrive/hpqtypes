{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module DB.Primitive.Interface where

import Foreign.C
import Foreign.Ptr

import DB.Primitive.Types

foreign import ccall unsafe "PQconnectdb"
  pqConnectDb :: CString -> IO (Ptr PGconn)

foreign import ccall unsafe "PQinitTypes"
  pqInitTypes :: Ptr PGconn -> IO ()

-- it may run for a long time, make it safe
foreign import ccall safe "PQparamExec"
  pqParamExec :: Ptr PGconn -> Ptr PGparam -> CString -> CInt -> IO (Ptr PGresult)

foreign import ccall unsafe "PQntuples"
  pqNTuples :: Ptr PGresult -> CInt

foreign import ccall unsafe "PQgetisnull"
  pqGetIsNull :: Ptr PGresult -> CInt -> CInt -> CInt

foreign import ccall unsafe "PQgeterror"
  pqGetError :: IO CString

foreign import ccall unsafe "&PQclear"
  pqClear :: FunPtr (Ptr PGresult -> IO ())

foreign import ccall unsafe "PQfinish"
  pqFinish :: Ptr PGconn -> IO ()
