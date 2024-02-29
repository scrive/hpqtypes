-- | Exports a set of FFI-imported libpq/libpqtypes functions.
module Database.PostgreSQL.PQTypes.Internal.C.Interface
  ( -- * libpq imports
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
  , c_PQconnectdb
  , c_PQfinish

    -- * libpqtypes imports
  , c_PQinitTypes
  , c_PQregisterTypes
  , c_PQparamExec
  , c_PQparamPrepare
  , c_PQparamExecPrepared
  , c_PQparamCreate
  , c_PQparamClear
  , c_PQparamCount

    -- * misc imports
  , nullStringCStringLen
  ) where

import Control.Exception qualified as E
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import System.Posix.Types

import Database.PostgreSQL.PQTypes.Internal.C.Types

----------------------------------------
-- PGconn

foreign import ccall unsafe "PQfreemem"
  c_PQfreemem :: Ptr a -> IO ()

foreign import ccall unsafe "PQstatus"
  c_PQstatus :: Ptr PGconn -> IO ConnStatusType

foreign import ccall unsafe "PQerrorMessage"
  c_PQerrorMessage :: Ptr PGconn -> IO CString

foreign import ccall unsafe "PQsocket"
  c_PQsocket :: Ptr PGconn -> IO Fd

-- | Safe as it sends a query to the server.
foreign import ccall safe "PQsetClientEncoding"
  c_PQsetClientEncoding :: Ptr PGconn -> CString -> IO CInt

-- | Safe as it reads data from a socket.
foreign import ccall safe "PQconsumeInput"
  c_PQconsumeInput :: Ptr PGconn -> IO CInt

-- | Safe as it connects to the database, which can take some time.
foreign import ccall safe "PQconnectdb"
  c_PQconnectdb :: CString -> IO (Ptr PGconn)

-- | Safe as it sends a terminate command to the server.
foreign import ccall safe "PQfinish"
  c_PQfinish :: Ptr PGconn -> IO ()

----------------------------------------
-- PGresult

foreign import ccall unsafe "PQresultStatus"
  c_PQresultStatus :: Ptr PGresult -> IO ExecStatusType

foreign import ccall unsafe "PQresultErrorField"
  c_PQresultErrorField :: Ptr PGresult -> ErrorField -> IO CString

foreign import ccall unsafe "PQresultErrorMessage"
  c_PQresultErrorMessage :: Ptr PGresult -> IO CString

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

-- | Safe as it performs multiple actions when clearing the result.
foreign import ccall safe "PQclear"
  c_PQclear :: Ptr PGresult -> IO ()

-- | Safe as it performs multiple actions when clearing the result.
foreign import ccall safe "&PQclear"
  c_ptr_PQclear :: FunPtr (Ptr PGresult -> IO ())

----------------------------------------
-- PGcancel

foreign import ccall unsafe "PQgetCancel"
  c_PQgetCancel :: Ptr PGconn -> IO (Ptr PGcancel)

foreign import ccall unsafe "PQfreeCancel"
  c_PQfreeCancel :: Ptr PGcancel -> IO ()

-- | Safe as it establishes a separate connection to PostgreSQL to send the
-- cancellation request.
foreign import ccall safe "PQcancel"
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
-- libpqtypes / PGparam

foreign import ccall unsafe "PQparamCreate"
  c_PQparamCreate :: Ptr PGconn -> Ptr PGerror -> IO (Ptr PGparam)

foreign import ccall unsafe "PQparamClear"
  c_PQparamClear :: Ptr PGparam -> IO ()

foreign import ccall unsafe "PQparamCount"
  c_PQparamCount :: Ptr PGparam -> IO CInt

-- | Safe as it calls PQregisterEventProc with a nontrivial callback.
foreign import ccall safe "PQinitTypes"
  c_PQinitTypes :: Ptr PGconn -> IO ()

-- | Safe as it sends a query to the server.
foreign import ccall safe "PQregisterTypes"
  c_PQregisterTypes :: Ptr PGconn -> Ptr PGerror -> TypeClass -> Ptr PGregisterType -> CInt -> CInt -> IO CInt

-- | Safe as query execution might run for a long time.
foreign import ccall safe "PQparamExec"
  c_rawPQparamExec :: Ptr PGconn -> Ptr PGerror -> Ptr PGparam -> CString -> ResultFormat -> IO (Ptr PGresult)

-- | Safe as it contacts the server.
foreign import ccall safe "PQparamPrepare"
  c_rawPQparamPrepare :: Ptr PGconn -> Ptr PGerror -> Ptr PGparam -> CString -> CString -> IO (Ptr PGresult)

-- | Safe as query execution might run for a long time.
foreign import ccall safe "PQparamExecPrepared"
  c_rawPQparamExecPrepared :: Ptr PGconn -> Ptr PGerror -> Ptr PGparam -> CString -> ResultFormat -> IO (Ptr PGresult)

-- | Safe wrapper for 'c_rawPQparamExec'. Wraps result returned by
-- 'c_rawPQparamExec' in 'ForeignPtr' with asynchronous exceptions masked to
-- prevent memory leaks.
c_PQparamExec :: Ptr PGconn -> Ptr PGerror -> Ptr PGparam -> CString -> ResultFormat -> IO (ForeignPtr PGresult)
c_PQparamExec conn err param fmt mode = do
  E.mask_ $
    newForeignPtr c_ptr_PQclear
      =<< c_rawPQparamExec conn err param fmt mode

-- | Safe wrapper for 'c_rawPQprepare'. Wraps result returned by
-- 'c_rawPQprepare' in 'ForeignPtr' with asynchronous exceptions masked to
-- prevent memory leaks.
c_PQparamPrepare
  :: Ptr PGconn
  -> Ptr PGerror
  -> Ptr PGparam
  -> CString
  -> CString
  -> IO (ForeignPtr PGresult)
c_PQparamPrepare conn err param queryName query = do
  E.mask_ $
    newForeignPtr c_ptr_PQclear
      =<< c_rawPQparamPrepare conn err param queryName query

-- | Safe wrapper for 'c_rawPQparamExecPrepared'. Wraps result returned by
-- 'c_rawPQparamExecPrepared' in 'ForeignPtr' with asynchronous exceptions
-- masked to prevent memory leaks.
c_PQparamExecPrepared
  :: Ptr PGconn
  -> Ptr PGerror
  -> Ptr PGparam
  -> CString
  -> ResultFormat
  -> IO (ForeignPtr PGresult)
c_PQparamExecPrepared conn err param queryName mode = do
  E.mask_ $
    newForeignPtr c_ptr_PQclear
      =<< c_rawPQparamExecPrepared conn err param queryName mode

----------------------------------------
-- Miscellaneous

foreign import ccall unsafe "&pqt_hs_null_string_ptr"
  nullStringPtr :: Ptr CChar

nullStringCStringLen :: CStringLen
nullStringCStringLen = (nullStringPtr, 0)
