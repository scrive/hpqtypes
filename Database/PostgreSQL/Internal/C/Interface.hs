{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Database.PostgreSQL.Internal.C.Interface where

import Foreign.C
import Foreign.ForeignPtr
import Foreign.Ptr
import qualified Control.Exception as E

import Database.PostgreSQL.Internal.C.Types

-- it may block in case of network problem, make it safe
foreign import ccall safe "PQconnectdb"
  c_PQconnectdb :: CString -> IO (Ptr PGconn)

foreign import ccall unsafe "PQstatus"
  c_PQstatus :: Ptr PGconn -> IO ConnStatusType

foreign import ccall unsafe "PQerrorMessage"
  c_PQerrorMessage :: Ptr PGconn -> IO CString

foreign import ccall unsafe "PQinitTypes"
  c_PQinitTypes :: Ptr PGconn -> IO ()

foreign import ccall unsafe "PQregisterTypes"
  c_PQregisterTypes :: Ptr PGconn -> TypeClass -> Ptr PGregisterType -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "PQparamCreate"
  c_PQparamCreate :: Ptr PGconn -> IO (Ptr PGparam)

-- it may run for a long time, make it safe
foreign import ccall safe "PQparamExec"
  c_PQparamExec :: Ptr PGconn -> Ptr PGparam -> CString -> CInt -> IO (Ptr PGresult)

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

foreign import ccall unsafe "PQgeterror"
  c_PQgeterror :: IO CString

foreign import ccall unsafe "PQparamClear"
  c_PQparamClear :: Ptr PGparam -> IO ()

foreign import ccall unsafe "PQclear"
  c_PQclear :: Ptr PGresult -> IO ()

foreign import ccall unsafe "PQfinish"
  c_PQfinish :: Ptr PGconn -> IO ()

foreign import ccall unsafe "&PQclear"
  c_ptr_PQclear :: FunPtr (Ptr PGresult -> IO ())

----------------------------------------

withPGparam :: Ptr PGconn -> (Ptr PGparam -> IO r) -> IO r
withPGparam conn = E.bracket (c_PQparamCreate conn) c_PQparamClear

c_safePQparamExec :: Ptr PGconn -> Ptr PGparam -> CString -> CInt -> IO (ForeignPtr PGresult)
c_safePQparamExec conn param fmt mode = E.mask_ $ newForeignPtr c_ptr_PQclear
  =<< c_PQparamExec conn param fmt mode
