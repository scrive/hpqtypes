-- | Exports a set of FFI-imported libpq/libpqtypes functions.
module Database.PostgreSQL.PQTypes.Internal.C.Interface (
  -- * libpq imports
    c_PQfreemem
  , c_PQstatus
  , c_PQerrorMessage
  , c_PQsetClientEncoding
  , c_PQsocket
  , c_PQconsumeInput
  , c_PQresultStatus
  , c_PQresultErrorField
  , c_PQresultErrorMessage
  , c_PQntuples
  , c_PQnfields
  , c_PQcmdTuples
  , c_PQgetisnull
  , c_PQfname
  , c_PQclear
  , c_PQcancel
  , c_PQconnectStart
  , c_PQconnectPoll
  , c_PQfinishPtr
  , c_ptr_PQfinishPtr
  -- * libpqtypes imports
  , c_PQinitTypes
  , c_PQregisterTypes
  , c_PQparamExec
  , c_PQparamCreate
  , c_PQparamClear
  , c_PQparamCount
  -- * misc imports
  , nullStringCStringLen
  )  where

import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import System.Posix.Types
import qualified Control.Exception as E

import Database.PostgreSQL.PQTypes.Internal.C.Types

-- libpq imports

foreign import capi unsafe "libpq-fe.h PQfreemem"
  c_PQfreemem :: Ptr a -> IO ()

foreign import capi unsafe "libpq-fe.h PQstatus"
  c_PQstatus :: Ptr PGconn -> IO ConnStatusType

foreign import capi unsafe "libpq-fe.h PQerrorMessage"
  c_PQerrorMessage :: Ptr PGconn -> IO CString

foreign import capi unsafe "libpq-fe.h PQsetClientEncoding"
  c_PQsetClientEncoding :: Ptr PGconn -> CString -> IO CInt

foreign import capi unsafe "libpq-fe.h PQsocket"
  c_PQsocket :: Ptr PGconn -> IO Fd

foreign import capi unsafe "libpq-fe.h PQconsumeInput"
  c_PQconsumeInput :: Ptr PGconn -> IO CInt

foreign import capi unsafe "libpq-fe.h PQresultStatus"
  c_PQresultStatus :: Ptr PGresult -> IO ExecStatusType

foreign import capi unsafe "libpq-fe.h PQresultErrorField"
  c_PQresultErrorField :: Ptr PGresult -> ErrorField -> IO CString

foreign import capi unsafe "libpq-fe.h PQresultErrorMessage"
  c_PQresultErrorMessage :: Ptr PGresult -> IO CString

foreign import capi unsafe "libpq-fe.h PQntuples"
  c_PQntuples :: Ptr PGresult -> IO CInt

foreign import capi unsafe "libpq-fe.h PQnfields"
  c_PQnfields :: Ptr PGresult -> IO CInt

foreign import capi unsafe "libpq-fe.h PQcmdTuples"
  c_PQcmdTuples :: Ptr PGresult -> IO CString

foreign import capi unsafe "libpq-fe.h PQgetisnull"
  c_PQgetisnull :: Ptr PGresult -> CInt -> CInt -> IO CInt

foreign import capi unsafe "libpq-fe.h PQfname"
  c_PQfname :: Ptr PGresult -> CInt -> IO CString

foreign import capi unsafe "libpq-fe.h PQclear"
  c_PQclear :: Ptr PGresult -> IO ()

----------------------------------------

foreign import capi unsafe "libpq-fe.h PQgetCancel"
  c_PQgetCancel :: Ptr PGconn -> IO (Ptr PGcancel)

foreign import capi unsafe "libpq-fe.h PQfreeCancel"
  c_PQfreeCancel :: Ptr PGcancel -> IO ()

foreign import capi unsafe "libpq-fe.h PQcancel"
  c_rawPQcancel :: Ptr PGcancel -> CString -> CInt -> IO CInt

-- | Attempt to cancel currently running query. If the request is successfully
-- dispatched Nothing is returned, otherwise a textual explanation of what
-- happened.
c_PQcancel :: Ptr PGconn -> IO (Maybe String)
c_PQcancel conn = E.bracket (c_PQgetCancel conn) c_PQfreeCancel $ \cancel -> do
  allocaBytes errbufsize $ \errbuf -> do
    c_rawPQcancel cancel errbuf (fromIntegral errbufsize) >>= \case
      0 -> Just <$> peekCString errbuf
      _ -> return Nothing
  where
    -- Size recommended by
    -- https://www.postgresql.org/docs/current/static/libpq-cancel.html
    errbufsize :: Int
    errbufsize = 256

----------------------------------------

foreign import capi unsafe "libpq-fe.h PQconnectStart"
  c_PQconnectStart :: CString -> IO (Ptr PGconn)

foreign import capi unsafe "libpq-fe.h PQconnectPoll"
  c_PQconnectPoll :: Ptr PGconn -> IO PostgresPollingStatusType

foreign import capi unsafe "libpqtypes.h PQfinishPtr"
  c_PQfinishPtr :: Ptr (Ptr PGconn) -> IO ()

foreign import capi unsafe "libpqtypes.h &PQfinishPtr"
  c_ptr_PQfinishPtr :: FunPtr (Ptr (Ptr PGconn) -> IO ())

-- libpqtypes imports

foreign import capi unsafe "libpqtypes.h PQinitTypes"
  c_PQinitTypes :: Ptr PGconn -> IO ()

foreign import capi unsafe "libpqtypes.h PQregisterTypes"
  c_PQregisterTypes :: Ptr PGconn -> Ptr PGerror -> TypeClass -> Ptr PGregisterType -> CInt -> CInt -> IO CInt

foreign import capi unsafe "libpqtypes.h PQparamCreate"
  c_PQparamCreate :: Ptr PGconn -> Ptr PGerror -> IO (Ptr PGparam)

foreign import capi unsafe "libpqtypes.h PQparamClear"
  c_PQparamClear :: Ptr PGparam -> IO ()

foreign import capi unsafe "libpqtypes.h PQparamCount"
  c_PQparamCount :: Ptr PGparam -> IO CInt

-- misc

foreign import capi unsafe "libpqtypes.h &pqt_hs_null_string_ptr"
  nullStringPtr :: Ptr CChar

nullStringCStringLen :: CStringLen
nullStringCStringLen = (nullStringPtr, 0)

----------------------------------------

-- | May run for a long time, hence marked 'safe'.
foreign import capi safe "libpqtypes.h PQparamExec"
  c_rawPQparamExec :: Ptr PGconn -> Ptr PGerror -> Ptr PGparam -> CString -> ResultFormat -> IO (Ptr PGresult)

foreign import capi unsafe "libpq-fe.h &PQclear"
  c_ptr_PQclear :: FunPtr (Ptr PGresult -> IO ())

-- | Safe wrapper for 'c_rawPQparamExec'. Wraps result returned by
-- 'c_rawPQparamExec' in 'ForeignPtr' with asynchronous exceptions
-- masked to prevent memory leaks.
c_PQparamExec :: Ptr PGconn -> Ptr PGerror -> Ptr PGparam -> CString -> ResultFormat -> IO (ForeignPtr PGresult)
c_PQparamExec conn err param fmt mode = E.mask_ $ newForeignPtr c_ptr_PQclear
  =<< c_rawPQparamExec conn err param fmt mode
