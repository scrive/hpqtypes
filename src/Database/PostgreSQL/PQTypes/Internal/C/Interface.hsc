-- | Exports a set of FFI-imported libpq functions.
module Database.PostgreSQL.PQTypes.Internal.C.Interface
  ( -- * libpq imports
    c_PQfreemem
  , c_PQstatus
  , c_PQtransactionStatus
  , c_PQerrorMessage
  , c_PQsetClientEncoding
  , c_PQprepare
  , c_PQsocket
  , c_PQbackendPid
  , c_PQconsumeInput
  , c_PQresultStatus
  , c_PQresultErrorField
  , c_PQntuples
  , c_PQnfields
  , c_PQcmdTuples
  , c_PQgetisnull
  , c_PQfname
  , c_PQftype
  , c_PQgetvalue
  , c_PQgetlength
  , c_PQclear
  , c_ptr_PQclear
  , c_PQcancel
  , c_PQconnectdb
  , c_PQfinish

    -- * asynchronous query processing
  , c_PQsendQueryParams
  , c_PQsendPrepare
  , c_PQsendQueryPrepared
  , c_PQsetnonblocking
  , c_PQflush
  , c_PQisBusy
  , c_PQgetResult

    -- * misc imports
  , nullStringPtr
  ) where

#include <libpq-fe.h>

import Control.Exception qualified as E
import Foreign.C
import Foreign.Ptr
import System.Posix.Types

import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Oid

#ifndef LIBPQ_HAS_ASYNC_CANCEL
import Foreign.Marshal.Alloc
#endif

----------------------------------------
-- PGconn

foreign import ccall unsafe "PQfreemem"
  c_PQfreemem :: Ptr a -> IO ()

foreign import ccall unsafe "PQstatus"
  c_PQstatus :: Ptr PGconn -> IO ConnStatusType

foreign import ccall unsafe "PQtransactionStatus"
  c_PQtransactionStatus :: Ptr PGconn -> IO PGTransactionStatusType

foreign import ccall unsafe "PQerrorMessage"
  c_PQerrorMessage :: Ptr PGconn -> IO CString

foreign import ccall unsafe "PQsocket"
  c_PQsocket :: Ptr PGconn -> IO Fd

foreign import ccall unsafe "PQbackendPID"
  c_PQbackendPid :: Ptr PGconn -> IO CInt

-- | Safe as it executes a query on the server and blocks waiting for its
-- result (synchronous libpq functions ignore the non-blocking mode of the
-- connection).
foreign import ccall safe "PQsetClientEncoding"
  c_PQsetClientEncoding :: Ptr PGconn -> CString -> IO CInt

-- | Safe as it executes a command on the server and blocks waiting for its
-- result (synchronous libpq functions ignore the non-blocking mode of the
-- connection).
foreign import ccall safe "PQprepare"
  c_PQprepare :: Ptr PGconn -> CString -> CString -> CInt -> Ptr Oid -> IO (Ptr PGresult)

-- | Safe as it drains data accumulated in the kernel buffer of the socket
-- (and decrypts it when TLS is in use), which can take a while (note that it
-- never blocks waiting for data though).
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

foreign import ccall unsafe "PQftype"
  c_PQftype :: Ptr PGresult -> CInt -> IO Oid

foreign import ccall unsafe "PQgetvalue"
  c_PQgetvalue :: Ptr PGresult -> CInt -> CInt -> IO CString

foreign import ccall unsafe "PQgetlength"
  c_PQgetlength :: Ptr PGresult -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "PQclear"
  c_PQclear :: Ptr PGresult -> IO ()

foreign import ccall unsafe "&PQclear"
  c_ptr_PQclear :: FunPtr (Ptr PGresult -> IO ())

----------------------------------------
-- Asynchronous query processing

-- | Safe as it copies the query and its parameters into the output buffer,
-- which can take a while for large inputs (note that it never blocks on the
-- socket, since connections are always in the non-blocking mode).
foreign import ccall safe "PQsendQueryParams"
  c_PQsendQueryParams
    :: Ptr PGconn
    -> CString
    -> CInt
    -> Ptr Oid
    -> Ptr CString
    -> Ptr CInt
    -> Ptr CInt
    -> ResultFormat
    -> IO CInt

-- | Safe as it copies the query and its parameters into the output buffer,
-- which can take a while for large inputs (note that it never blocks on the
-- socket, since connections are always in the non-blocking mode).
foreign import ccall safe "PQsendPrepare"
  c_PQsendPrepare
    :: Ptr PGconn
    -> CString
    -> CString
    -> CInt
    -> Ptr Oid
    -> IO CInt

-- | Safe as it copies the query and its parameters into the output buffer,
-- which can take a while for large inputs (note that it never blocks on the
-- socket, since connections are always in the non-blocking mode).
foreign import ccall safe "PQsendQueryPrepared"
  c_PQsendQueryPrepared
    :: Ptr PGconn
    -> CString
    -> CInt
    -> Ptr CString
    -> Ptr CInt
    -> Ptr CInt
    -> ResultFormat
    -> IO CInt

-- | Unsafe only because it's exclusively called with the second argument set
-- to 1, which merely sets a flag (setting it to 0 attempts to flush the
-- output buffer, blocking on the socket if necessary).
foreign import ccall unsafe "PQsetnonblocking"
  c_PQsetnonblocking :: Ptr PGconn -> CInt -> IO CInt

-- | Safe as it pushes data from the output buffer into the kernel buffer of
-- the socket (encrypting it first when TLS is in use), which can take a while
-- (note that it never blocks waiting for the socket to become writable
-- though, since connections are always in the non-blocking mode).
foreign import ccall safe "PQflush"
  c_PQflush :: Ptr PGconn -> IO CInt

foreign import ccall unsafe "PQisBusy"
  c_PQisBusy :: Ptr PGconn -> IO CInt

-- | Safe as it blocks waiting for data from the server if the whole result
-- is not yet available (which doesn't happen if it's called only when
-- 'c_PQisBusy' returns 0, but better safe than sorry) and constructing the
-- result involves copying the accumulated data, which can take a while.
foreign import ccall safe "PQgetResult"
  c_PQgetResult :: Ptr PGconn -> IO (Ptr PGresult)

----------------------------------------
-- Query cancellation

#ifdef LIBPQ_HAS_ASYNC_CANCEL

foreign import ccall unsafe "PQcancelCreate"
  c_PQcancelCreate :: Ptr PGconn -> IO (Ptr PGcancelConn)

-- | Safe as it establishes a separate connection to PostgreSQL to send the
-- cancellation request and waits until the server acknowledges its receipt
-- by closing that connection.
foreign import ccall safe "PQcancelBlocking"
  c_PQcancelBlocking :: Ptr PGcancelConn -> IO CInt

foreign import ccall unsafe "PQcancelErrorMessage"
  c_PQcancelErrorMessage :: Ptr PGcancelConn -> IO CString

foreign import ccall unsafe "PQcancelFinish"
  c_PQcancelFinish :: Ptr PGcancelConn -> IO ()

-- | Attempt to cancel the currently running query. Returns once the server
-- acknowledged the receipt of the cancellation request; Nothing on success,
-- otherwise a textual explanation of what happened.
c_PQcancel :: Ptr PGconn -> IO (Maybe String)
c_PQcancel conn = do
  E.bracket (c_PQcancelCreate conn) c_PQcancelFinish $ \cancelConn ->
    if cancelConn == nullPtr
    then pure $ Just "PQcancelCreate returned a null pointer"
    else c_PQcancelBlocking cancelConn >>= \case
      0 -> Just <$> (peekCString =<< c_PQcancelErrorMessage cancelConn)
      _ -> pure Nothing

#else

foreign import ccall unsafe "PQgetCancel"
  c_PQgetCancel :: Ptr PGconn -> IO (Ptr PGcancel)

foreign import ccall unsafe "PQfreeCancel"
  c_PQfreeCancel :: Ptr PGcancel -> IO ()

-- | Safe as it establishes a separate connection to PostgreSQL to send the
-- cancellation request and waits until the server acknowledges its receipt
-- by closing that connection.
foreign import ccall safe "PQcancel"
  c_rawPQcancel :: Ptr PGcancel -> CString -> CInt -> IO CInt

-- | Attempt to cancel the currently running query. Returns once the server
-- acknowledged the receipt of the cancellation request; Nothing on success,
-- otherwise a textual explanation of what happened.
c_PQcancel :: Ptr PGconn -> IO (Maybe String)
c_PQcancel conn = E.bracket (c_PQgetCancel conn) c_PQfreeCancel $ \cancel -> do
  allocaBytes errbufsize $ \errbuf -> do
    c_rawPQcancel cancel errbuf (fromIntegral errbufsize) >>= \case
      0 -> Just <$> peekCString errbuf
      _ -> pure Nothing
  where
    -- Size recommended by
    -- https://www.postgresql.org/docs/current/static/libpq-cancel.html
    errbufsize :: Int
    errbufsize = 256

#endif

----------------------------------------
-- Miscellaneous

-- | A non-null pointer to an empty C string, for passing an empty value to
-- libpq in case the input ByteString is backed by a null pointer, which
-- libpq would interpret as SQL NULL.
foreign import ccall unsafe "&hpqtypes_null_string_ptr"
  nullStringPtr :: CString
