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
  -- * libpqtypes imports
  , c_PQfinishPtr
  , c_PQconnectdb
  , c_PQinitTypes
  , c_PQregisterTypes
  , c_PQparamExec
  , c_PQparamCreate
  , c_PQparamClear
  , c_PQparamCount
  -- * misc imports
  , nullStringCStringLen
  )  where

import Control.Applicative
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Prelude
import System.Posix.Types
import qualified Control.Exception as E

import Database.PostgreSQL.PQTypes.Internal.C.Types

-- libpq imports

foreign import ccall unsafe "PQfreemem"
  c_PQfreemem :: Ptr a -> IO ()

foreign import ccall unsafe "PQstatus"
  c_PQstatus :: Ptr PGconn -> IO ConnStatusType

foreign import ccall unsafe "PQerrorMessage"
  c_PQerrorMessage :: Ptr PGconn -> IO CString

foreign import ccall unsafe "PQsetClientEncoding"
  c_PQsetClientEncoding :: Ptr PGconn -> CString -> IO CInt

foreign import ccall unsafe "PQsocket"
  c_PQsocket :: Ptr PGconn -> IO Fd

foreign import ccall unsafe "PQconsumeInput"
  c_PQconsumeInput :: Ptr PGconn -> IO CInt

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

foreign import ccall unsafe "PQclear"
  c_PQclear :: Ptr PGresult -> IO ()

----------------------------------------

foreign import ccall unsafe "PQgetCancel"
  c_PQgetCancel :: Ptr PGconn -> IO (Ptr PGcancel)

foreign import ccall unsafe "PQfreeCancel"
  c_PQfreeCancel :: Ptr PGcancel -> IO ()

foreign import ccall unsafe "PQcancel"
  c_rawPQcancel :: Ptr PGcancel -> CString -> CInt -> IO CInt

-- | Attempt to cancel currently running query. If the request is successfully
-- dispatched Nothing is returned, otherwise a textual explanation of what
-- happened.
c_PQcancel :: Ptr PGconn -> IO (Maybe String)
c_PQcancel conn = E.mask $ \restore -> do
  cancel <- c_PQgetCancel conn
  (`E.finally` c_PQfreeCancel cancel) . restore $ do
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

-- | May block in case of network problems, hence marked 'safe'.
foreign import ccall safe "PQconnectdb"
  c_rawPQconnectdb :: CString -> IO (Ptr PGconn)

foreign import ccall unsafe "PQfinishPtr"
  c_PQfinishPtr :: Ptr (Ptr PGconn) -> IO ()

foreign import ccall unsafe "&PQfinishPtr"
  c_ptr_PQfinishPtr :: FunPtr (Ptr (Ptr PGconn) -> IO ())

-- | Safe wrapper for 'c_rawPQconnectdb', returns
-- 'ForeignPtr' instead of 'Ptr'.
c_PQconnectdb :: CString -> IO (ForeignPtr (Ptr PGconn))
c_PQconnectdb conninfo = E.mask_ $ do
  conn <- c_rawPQconnectdb conninfo
  -- Work around a bug in GHC that causes foreign pointer
  -- finalizers to be run multiple times under random
  -- circumstances by providing another level of indirection
  -- and a wrapper for PQfinish that can be safely called
  -- multiple times.
  connPtr <- mallocForeignPtr
  withForeignPtr connPtr $ flip poke conn
  addForeignPtrFinalizer c_ptr_PQfinishPtr connPtr
  return connPtr

-- libpqtypes imports

foreign import ccall unsafe "PQinitTypes"
  c_PQinitTypes :: Ptr PGconn -> IO ()

foreign import ccall unsafe "PQregisterTypes"
  c_PQregisterTypes :: Ptr PGconn -> Ptr PGerror -> TypeClass -> Ptr PGregisterType -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "PQparamCreate"
  c_PQparamCreate :: Ptr PGconn -> Ptr PGerror -> IO (Ptr PGparam)

foreign import ccall unsafe "PQparamClear"
  c_PQparamClear :: Ptr PGparam -> IO ()

foreign import ccall unsafe "PQparamCount"
  c_PQparamCount :: Ptr PGparam -> IO CInt

-- misc

foreign import ccall unsafe "&pqt_hs_null_string_ptr"
  nullStringPtr :: Ptr CChar

nullStringCStringLen :: CStringLen
nullStringCStringLen = (nullStringPtr, 0)

----------------------------------------

-- | May run for a long time, hence marked 'safe'.
foreign import ccall safe "PQparamExec"
  c_rawPQparamExec :: Ptr PGconn -> Ptr PGerror -> Ptr PGparam -> CString -> ResultFormat -> IO (Ptr PGresult)

foreign import ccall unsafe "&PQclear"
  c_ptr_PQclear :: FunPtr (Ptr PGresult -> IO ())

-- | Safe wrapper for 'c_rawPQparamExec'. Wraps result returned by
-- 'c_rawPQparamExec' in 'ForeignPtr' with asynchronous exceptions
-- masked to prevent memory leaks.
c_PQparamExec :: Ptr PGconn -> Ptr PGerror -> Ptr PGparam -> CString -> ResultFormat -> IO (ForeignPtr PGresult)
c_PQparamExec conn err param fmt mode = E.mask_ $ newForeignPtr c_ptr_PQclear
  =<< c_rawPQparamExec conn err param fmt mode
