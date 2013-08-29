{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module DB.Primitive.Interface where

import Foreign.C
import Foreign.ForeignPtr
import Foreign.Ptr
import qualified Control.Exception as E

import DB.Primitive.Types

foreign import ccall unsafe "PQconnectdb"
  c_PQconnectdb :: CString -> IO (Ptr PGconn)

foreign import ccall unsafe "PQinitTypes"
  c_PQinitTypes :: Ptr PGconn -> IO ()

foreign import ccall unsafe "PQparamCreate"
  c_PQparamCreate :: Ptr PGconn -> IO (Ptr PGparam)

-- it may run for a long time, make it safe
foreign import ccall safe "PQparamExec"
  c_PQparamExec :: Ptr PGconn -> Ptr PGparam -> CString -> CInt -> IO (Ptr PGresult)

foreign import ccall unsafe "PQntuples"
  c_PQntuples :: Ptr PGresult -> IO CInt

foreign import ccall unsafe "PQgetisnull"
  c_PQgetisnull :: Ptr PGresult -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "PQgeterror"
  c_PQgeterror :: IO CString

foreign import ccall unsafe "&PQparamClear"
  c_PQparamClear :: FunPtr (Ptr PGparam -> IO ())

foreign import ccall unsafe "&PQclear"
  c_PQclear :: FunPtr (Ptr PGresult -> IO ())

foreign import ccall unsafe "PQfinish"
  c_PQfinish :: Ptr PGconn -> IO ()

----------------------------------------

pqParamCreate :: Ptr PGconn -> IO (ForeignPtr PGparam)
pqParamCreate conn = E.mask_ $ newForeignPtr c_PQparamClear
  =<< c_PQparamCreate conn

pqParamExec :: Ptr PGconn -> Ptr PGparam -> CString -> CInt -> IO (ForeignPtr PGresult)
pqParamExec conn param fmt mode = E.mask_ $ newForeignPtr c_PQclear
  =<< c_PQparamExec conn param fmt mode
