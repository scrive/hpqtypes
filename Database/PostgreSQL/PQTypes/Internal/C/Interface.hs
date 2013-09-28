{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Database.PostgreSQL.PQTypes.Internal.C.Interface where

import Foreign.C
import Foreign.ForeignPtr
import Foreign.Ptr
import qualified Control.Exception as E

import Database.PostgreSQL.PQTypes.Internal.C.Types

-- libpq imports

-- it may block in case of network problem, make it safe
foreign import ccall safe "PQconnectdb"
  c_PQconnectdb :: CString -> IO (Ptr PGconn)

foreign import ccall unsafe "PQstatus"
  c_PQstatus :: Ptr PGconn -> IO ConnStatusType

foreign import ccall unsafe "PQerrorMessage"
  c_PQerrorMessage :: Ptr PGconn -> IO CString

foreign import ccall unsafe "PQsetClientEncoding"
  c_PQsetClientEncoding :: Ptr PGconn -> CString -> IO CInt

foreign import ccall unsafe "PQresultStatus"
  c_PQresultStatus :: Ptr PGresult -> IO ExecStatusType

foreign import ccall unsafe "PQntuples"
  c_PQntuples :: Ptr PGresult -> IO CInt

foreign import ccall unsafe "PQnfields"
  c_PQnfields :: Ptr PGresult -> IO CInt

foreign import ccall unsafe "PQcmdTuples"
  c_PQcmdTuples :: Ptr PGresult -> IO CString

foreign import ccall unsafe "PQgetisnull"
  c_PQgetisnull :: Ptr PGresult -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "PQfname"
  c_PQfname :: Ptr PGresult -> CInt -> IO CString

foreign import ccall unsafe "PQclear"
  c_PQclear :: Ptr PGresult -> IO ()

foreign import ccall unsafe "PQfinish"
  c_PQfinish :: Ptr PGconn -> IO ()

foreign import ccall unsafe "&PQclear"
  c_ptr_PQclear :: FunPtr (Ptr PGresult -> IO ())

-- libpqtypes imports

foreign import ccall unsafe "PQinitTypes"
  c_PQinitTypes :: Ptr PGconn -> IO ()

foreign import ccall unsafe "PQregisterTypes"
  c_PQregisterTypes :: Ptr PGconn -> Ptr PGerror -> TypeClass -> Ptr PGregisterType -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "PQparamCreate"
  c_PQparamCreate :: Ptr PGconn -> Ptr PGerror -> IO (Ptr PGparam)

foreign import ccall unsafe "PQparamClear"
  c_PQparamClear :: Ptr PGparam -> IO ()

-- it may run for a long time, make it safe
foreign import ccall safe "PQparamExec"
  c_rawPQparamExec :: Ptr PGconn -> Ptr PGerror -> Ptr PGparam -> CString -> ResultFormat -> IO (Ptr PGresult)

c_PQparamExec :: Ptr PGconn -> Ptr PGerror -> Ptr PGparam -> CString -> ResultFormat -> IO (ForeignPtr PGresult)
c_PQparamExec conn err param fmt mode = E.mask_ $ newForeignPtr c_ptr_PQclear
  =<< c_rawPQparamExec conn err param fmt mode
