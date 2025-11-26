module Database.PostgreSQL.PQTypes.Internal.Connection
  ( -- * Connection
    Connection (..)
  , ConnectionStats (..)
  , initialConnectionStats
  , ConnectionSettings (..)
  , defaultConnectionSettings
  , ConnectionSourceM (..)
  , InternalConnectionSource (..)
  , ConnectionSource (..)
  , simpleSource
  , poolSource
  , connect
  , disconnect

    -- * Running queries
  , runQueryIO
  , QueryName (..)
  , runPreparedQueryIO
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception qualified as E
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Data.ByteString.Char8 qualified as BS
import Data.Foldable qualified as F
import Data.Functor.Identity
import Data.IORef
import Data.Int
import Data.Kind
import Data.Maybe
import Data.Pool
import Data.Set qualified as S
import Data.String
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Ptr
import GHC.Clock (getMonotonicTime)
import GHC.Conc (closeFdWith)
import GHC.Stack

import Database.PostgreSQL.PQTypes.Internal.BackendPid
import Database.PostgreSQL.PQTypes.Internal.C.Interface
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Composite
import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.Internal.Error.Code
import Database.PostgreSQL.PQTypes.Internal.Exception
import Database.PostgreSQL.PQTypes.Internal.QueryResult
import Database.PostgreSQL.PQTypes.Internal.Utils
import Database.PostgreSQL.PQTypes.SQL.Class
import Database.PostgreSQL.PQTypes.SQL.Raw
import Database.PostgreSQL.PQTypes.ToSQL

data ConnectionSettings = ConnectionSettings
  { csConnInfo :: !T.Text
  -- ^ Connection info string.
  , csClientEncoding :: !(Maybe T.Text)
  -- ^ Client-side encoding. If set to 'Nothing', database encoding is used.
  , csRole :: !(Maybe (RawSQL ()))
  -- ^ A custom role to set with "SET ROLE".
  , csComposites :: ![T.Text]
  -- ^ A list of composite types to register. In order to be able to
  -- (de)serialize specific composite types, you need to register them.
  }
  deriving (Eq, Ord, Show)

-- | Default connection settings. Note that all strings sent to PostgreSQL by
-- the library are encoded as UTF-8, so don't alter client encoding unless you
-- know what you're doing.
defaultConnectionSettings :: ConnectionSettings
defaultConnectionSettings =
  ConnectionSettings
    { csConnInfo = T.empty
    , csClientEncoding = Just "UTF-8"
    , csRole = Nothing
    , csComposites = []
    }

----------------------------------------

-- | Simple connection statistics.
data ConnectionStats = ConnectionStats
  { statsQueries :: !Int
  -- ^ Number of queries executed so far.
  , statsRows :: !Int
  -- ^ Number of rows fetched from the database.
  , statsValues :: !Int
  -- ^ Number of values fetched from the database.
  , statsParams :: !Int
  -- ^ Number of parameters sent to the database.
  , statsTime :: !Double
  -- ^ Time spent executing queries (in seconds).
  }
  deriving (Eq, Ord, Show)

-- | Initial connection statistics.
initialConnectionStats :: ConnectionStats
initialConnectionStats =
  ConnectionStats
    { statsQueries = 0
    , statsRows = 0
    , statsValues = 0
    , statsParams = 0
    , statsTime = 0
    }

-- | Representation of a connection object.
--
-- /Note:/ PGconn is not managed with a ForeignPtr because finalizers are broken
-- and at program exit might run even though another thread is inside the
-- relevant withForeignPtr block, executing a safe FFI call (in this case
-- executing an SQL query).
--
-- See https://gitlab.haskell.org/ghc/ghc/-/issues/10975 for more info.
data Connection = Connection
  { connPtr :: !(Ptr PGconn)
  -- ^ Pointer to connection object.
  , connBackendPid :: !BackendPid
  -- ^ Process ID of the server process attached to the current session.
  , connPreparedQueries :: !(IORef (S.Set T.Text))
  -- ^ A set of named prepared statements of the connection.
  }

data InternalConnectionSource m cdata = InternalConnectionSource
  { takeConnection :: !(m (Connection, cdata))
  , putConnection :: !(forall r. (Connection, cdata) -> ExitCase r -> m ())
  }

-- | Database connection supplier.
data ConnectionSourceM m
  = forall cdata. ConnectionSourceM !(InternalConnectionSource m cdata)

-- | Wrapper for a polymorphic connection source.
newtype ConnectionSource (cs :: [(Type -> Type) -> Constraint]) = ConnectionSource
  { unConnectionSource :: forall m. MkConstraint m cs => ConnectionSourceM m
  }

-- | Default connection supplier. It establishes new database connection each
-- time 'withConnection' is called.
simpleSource
  :: ConnectionSettings
  -> ConnectionSource [MonadBase IO, MonadMask]
simpleSource cs =
  ConnectionSource $
    ConnectionSourceM
      InternalConnectionSource
        { takeConnection = (,()) <$> liftBase (connect cs)
        , putConnection = \(conn, ()) _ -> liftBase $ disconnect conn
        }

-- | Pooled source. It uses striped pool from @resource-pool@ package to cache
-- established connections and reuse them.
poolSource
  :: ConnectionSettings
  -> (IO Connection -> (Connection -> IO ()) -> PoolConfig Connection)
  -- ^ A function for creating the 'PoolConfig' with desired parameters.
  --
  -- /Note:/ supplied arguments are for creation and destruction of a database
  -- connection.
  -> IO (ConnectionSource [MonadBase IO, MonadMask])
poolSource cs mkPoolConfig = do
  pool <- newPool $ mkPoolConfig (connect cs) disconnect
  pure $ ConnectionSource (sourceM pool)
  where
    sourceM pool =
      ConnectionSourceM
        InternalConnectionSource
          { takeConnection = liftBase $ takeResource pool
          , putConnection = \(resource, local) -> \case
              ExitCaseSuccess _ -> liftBase $ putResource local resource
              _ -> liftBase $ destroyResource pool local resource
          }

----------------------------------------

-- | Low-level function for connecting to the database. Useful if one wants to
-- implement custom connection source.
--
-- /Warning:/ the 'Connection' needs to be explicitly destroyed with
-- 'disconnect', otherwise there will be a resource leak.
connect :: ConnectionSettings -> IO Connection
connect ConnectionSettings {..} = mask $ \unmask -> do
  connPtr <- BS.useAsCString (T.encodeUtf8 csConnInfo) (openConnection unmask)
  (`onException` c_PQfinish connPtr) . unmask $ do
    status <- c_PQstatus connPtr
    when (status /= c_CONNECTION_OK) $
      throwLibPQError connPtr fname
    F.forM_ csClientEncoding $ \enc -> do
      res <- BS.useAsCString (T.encodeUtf8 enc) (c_PQsetClientEncoding connPtr)
      when (res == -1) $
        throwLibPQError connPtr fname
    c_PQinitTypes connPtr
    registerComposites connPtr csComposites
    conn <- do
      preparedQueries <- newIORef S.empty
      pure
        Connection
          { connPtr = connPtr
          , connBackendPid = noBackendPid
          , connPreparedQueries = preparedQueries
          }
    F.forM_ csRole $ \role -> runQueryIO conn $ "SET ROLE " <> role

    let selectPid = "SELECT pg_backend_pid()" :: RawSQL ()
    (_, res, _) <- runQueryIO conn selectPid
    case F.toList $ mkQueryResult @(Identity Int32) selectPid noBackendPid res of
      [pid] -> pure $ conn {connBackendPid = BackendPid $ fromIntegral pid}
      pids -> do
        let err = HPQTypesError $ "unexpected backend pid: " ++ show pids
        rethrowWithContext selectPid noBackendPid $ toException err
  where
    fname = "connect"

    openConnection :: (forall r. IO r -> IO r) -> CString -> IO (Ptr PGconn)
    openConnection unmask conninfo = do
      -- We use synchronous version of connecting to the database using
      -- 'PQconnectdb' instead of 'PQconnectStart' and 'PQconnectPoll', because
      -- the second method doesn't properly support the connect_timeout
      -- parameter from the connection string nor multihost setups.
      --
      -- The disadvantage of this is that a call to 'PQconnectdb' cannot be
      -- interrupted if the Haskell thread running it receives an asynchronous
      -- exception, so to guarantee prompt return in such scenario 'PQconnectdb'
      -- is run in a separate child thread. If the parent receives an exception
      -- while the child still runs, the child is signaled to clean up after
      -- itself and left behind.
      connVar <- newEmptyTMVarIO
      runningVar <- newTVarIO True
      _ <- forkIO $ do
        conn <- c_PQconnectdb conninfo
        join . atomically $
          readTVar runningVar >>= \case
            True -> do
              putTMVar connVar conn
              pure $ pure ()
            False -> pure $ c_PQfinish conn
      conn <-
        atomically (takeTMVar connVar) `onException` do
          join . atomically $ do
            writeTVar runningVar False
            maybe (pure ()) c_PQfinish <$> tryTakeTMVar connVar
      (`onException` c_PQfinish conn) . unmask $ do
        when (conn == nullPtr) $ do
          throwError "PQconnectdb returned a null pointer"
        status <- c_PQstatus conn
        when (status /= c_CONNECTION_OK) $ do
          merr <- c_PQerrorMessage conn >>= safePeekCString
          let reason = maybe "" (": " <>) merr
          throwError $ "openConnection failed" <> reason
        pure conn
      where
        throwError :: String -> IO a
        throwError = hpqTypesError . (fname ++) . (": " ++)

-- | Low-level function for disconnecting from the database. Useful if one wants
-- to implement custom connection source.
disconnect :: Connection -> IO ()
disconnect Connection {..} = do
  -- This covers the case when a connection is closed while other Haskell
  -- threads are using GHC's IO manager to wait on the descriptor. This is
  -- commonly the case with asynchronous notifications, for example. Since libpq
  -- is responsible for opening and closing the file descriptor, GHC's IO
  -- manager needs to be informed that the file descriptor has been closed. The
  -- IO manager will then raise an exception in those threads.
  c_PQsocket connPtr >>= \case
    -1 -> c_PQfinish connPtr -- can happen if the connection is bad/lost
    fd -> closeFdWith (\_ -> c_PQfinish connPtr) fd

----------------------------------------
-- Query running

-- | Low-level function for running an SQL query.
runQueryIO
  :: (HasCallStack, IsSQL sql)
  => Connection
  -> sql
  -> IO (Int, ForeignPtr PGresult, ConnectionStats -> ConnectionStats)
runQueryIO conn@Connection {..} sql = do
  runQueryImpl conn sql $ do
    let allocParam = ParamAllocator $ withPGparam connPtr
    withSQL sql allocParam $ \param query ->
      (,)
        <$> (fromIntegral <$> c_PQparamCount param)
        <*> c_PQparamExec connPtr nullPtr param query c_RESULT_BINARY

-- | Name of a prepared query.
newtype QueryName = QueryName T.Text
  deriving (Eq, Ord, Show, IsString)

-- | Low-level function for running a prepared SQL query.
runPreparedQueryIO
  :: (HasCallStack, IsSQL sql)
  => Connection
  -> QueryName
  -> sql
  -> IO (Int, ForeignPtr PGresult, ConnectionStats -> ConnectionStats)
runPreparedQueryIO conn@Connection {..} (QueryName queryName) sql = do
  runQueryImpl conn sql $ do
    when (T.null queryName) $ do
      E.throwIO
        DBException
          { dbeQueryContext = sql
          , dbeBackendPid = connBackendPid
          , dbeError = HPQTypesError "runPreparedQueryIO: unnamed prepared query is not supported"
          , dbeCallStack = callStack
          }
    let allocParam = ParamAllocator $ withPGparam connPtr
    withSQL sql allocParam $ \param query -> do
      preparedQueries <- readIORef connPreparedQueries
      BS.useAsCString (T.encodeUtf8 queryName) $ \cname -> do
        when (queryName `S.notMember` preparedQueries) . E.mask_ $ do
          -- Mask asynchronous exceptions, because if preparation of the query
          -- succeeds, we need to reflect that fact in cdPreparedQueries since
          -- you can't prepare a query with the same name more than once.
          res <- c_PQparamPrepare connPtr nullPtr param cname query
          void . withForeignPtr res $ verifyResult sql connBackendPid connPtr
          modifyIORef' connPreparedQueries $ S.insert queryName
        (,)
          <$> (fromIntegral <$> c_PQparamCount param)
          <*> c_PQparamExecPrepared connPtr nullPtr param cname c_RESULT_BINARY

-- | Shared implementation of 'runQueryIO' and 'runPreparedQueryIO'.
runQueryImpl
  :: (HasCallStack, IsSQL sql)
  => Connection
  -> sql
  -> IO (Int, ForeignPtr PGresult)
  -> IO (Int, ForeignPtr PGresult, ConnectionStats -> ConnectionStats)
runQueryImpl Connection {..} sql execSql = do
  E.getMaskingState >>= \case
    E.MaskedUninterruptible -> do
      -- If asynchronous exceptions are already hard-masked, skip spawning a
      -- separate worker thread and the interruption logic as it won't do
      -- anything at this point.
      doRunQuery
    _ -> E.uninterruptibleMask $ \restore -> do
      -- While the query runs, the current thread will not be able to receive
      -- asynchronous exceptions. This prevents clients of the library from
      -- interrupting execution of the query. To remedy that we spawn a separate
      -- thread for the query execution and while we wait for its completion, we
      -- are able to receive asynchronous exceptions (assuming that threaded GHC
      -- runtime system is used) and react appropriately.
      queryRunner <- asyncWithUnmask $ \unmask -> do
        -- Unconditionally unmask asynchronous exceptions here so that 'cancel'
        -- operation potentially invoked below works as expected.
        unmask doRunQuery
      -- If we receive an exception while waiting for the execution to complete,
      -- we need to send a request to PostgreSQL for query cancellation and wait
      -- for the query runner thread to terminate. It is paramount we make the
      -- exception handler uninterruptible as we can't exit from the main block
      -- until the query runner thread has terminated.
      E.onException (restore $ wait queryRunner) $ do
        c_PQcancel connPtr >>= \case
          -- If query cancellation request was successfully processed, there is
          -- nothing else to do apart from waiting for the runner to terminate.
          Nothing -> cancel queryRunner
          -- Otherwise we check what happened with the runner. If it already
          -- finished we're fine, just ignore the result. If it didn't, something
          -- weird is going on. Maybe the cancellation request went through when
          -- the thread wasn't making a request to the server? In any case, try to
          -- cancel again and wait for the thread to terminate.
          Just _ ->
            poll queryRunner >>= \case
              Just _ -> pure ()
              Nothing -> do
                void $ c_PQcancel connPtr
                cancel queryRunner
  where
    doRunQuery = do
      t1 <- getMonotonicTime
      (paramCount, res) <- execSql
      t2 <- getMonotonicTime
      affected <- withForeignPtr res $ verifyResult sql connBackendPid connPtr
      updateStats <- case affected of
        Left _ ->
          pure $ \stats ->
            stats
              { statsQueries = statsQueries stats + 1
              , statsParams = statsParams stats + paramCount
              , statsTime = statsTime stats + (t2 - t1)
              }
        Right rows -> do
          columns <- fromIntegral <$> withForeignPtr res c_PQnfields
          pure $ \stats ->
            ConnectionStats
              { statsQueries = statsQueries stats + 1
              , statsRows = statsRows stats + rows
              , statsValues = statsValues stats + (rows * columns)
              , statsParams = statsParams stats + paramCount
              , statsTime = statsTime stats + (t2 - t1)
              }
      pure (either id id affected, res, updateStats)

verifyResult
  :: (HasCallStack, IsSQL sql)
  => sql
  -> BackendPid
  -> Ptr PGconn
  -> Ptr PGresult
  -> IO (Either Int Int)
verifyResult sql pid conn res = do
  -- works even if res is NULL
  rst <- c_PQresultStatus res
  case rst of
    _ | rst == c_PGRES_COMMAND_OK -> do
      sn <- c_PQcmdTuples res >>= BS.packCString
      case BS.readInt sn of
        Nothing
          | BS.null sn -> pure . Left $ 0
          | otherwise -> throwParseError sn
        Just (n, rest)
          | rest /= BS.empty -> throwParseError sn
          | otherwise -> pure . Left $ n
    _ | rst == c_PGRES_TUPLES_OK -> Right . fromIntegral <$> c_PQntuples res
    _ | rst == c_PGRES_FATAL_ERROR -> throwSQLError
    _ | rst == c_PGRES_BAD_RESPONSE -> throwSQLError
    _ | otherwise -> pure . Left $ 0
  where
    throwSQLError =
      rethrowWithContext sql pid
        =<< if res == nullPtr
          then
            E.toException . QueryError <$> (safePeekCString' =<< c_PQerrorMessage conn)
          else
            E.toException
              <$> ( DetailedQueryError
                      <$> field c_PG_DIAG_SEVERITY
                      <*> (stringToErrorCode <$> field c_PG_DIAG_SQLSTATE)
                      <*> field c_PG_DIAG_MESSAGE_PRIMARY
                      <*> mfield c_PG_DIAG_MESSAGE_DETAIL
                      <*> mfield c_PG_DIAG_MESSAGE_HINT
                      <*> ((mread =<<) <$> mfield c_PG_DIAG_STATEMENT_POSITION)
                      <*> ((mread =<<) <$> mfield c_PG_DIAG_INTERNAL_POSITION)
                      <*> mfield c_PG_DIAG_INTERNAL_QUERY
                      <*> mfield c_PG_DIAG_CONTEXT
                      <*> mfield c_PG_DIAG_SOURCE_FILE
                      <*> ((mread =<<) <$> mfield c_PG_DIAG_SOURCE_LINE)
                      <*> mfield c_PG_DIAG_SOURCE_FUNCTION
                  )
      where
        field f = fromMaybe "" <$> mfield f
        mfield f = safePeekCString =<< c_PQresultErrorField res f

    throwParseError sn =
      E.throwIO
        DBException
          { dbeQueryContext = sql
          , dbeBackendPid = pid
          , dbeError = HPQTypesError ("verifyResult: string returned by PQcmdTuples is not a valid number: " ++ show sn)
          , dbeCallStack = callStack
          }
