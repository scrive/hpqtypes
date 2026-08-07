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
import Control.Concurrent.STM
import Control.Exception qualified as E
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Unsafe qualified as BS
import Data.Foldable qualified as F
import Data.IORef
import Data.Kind
import Data.Maybe
import Data.Pool
import Data.Set qualified as S
import Data.String
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import GHC.Clock (getMonotonicTime)
import GHC.Conc (closeFdWith)
import GHC.Stack

import Database.PostgreSQL.PQTypes.Internal.BackendPid
import Database.PostgreSQL.PQTypes.Internal.C.Interface
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.Internal.Error.Code
import Database.PostgreSQL.PQTypes.Internal.Exception
import Database.PostgreSQL.PQTypes.Internal.Oid
import Database.PostgreSQL.PQTypes.Internal.Utils
import Database.PostgreSQL.PQTypes.SQL.Class
import Database.PostgreSQL.PQTypes.SQL.Raw
import Database.PostgreSQL.PQTypes.ToSQL

data ConnectionSettings = ConnectionSettings
  { csConnInfo :: !T.Text
  -- ^ Connection info string, see
  -- <https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING>
  -- for details. It's advisable to set the @connect_timeout@,
  -- @keepalives_idle@ and (on Linux) @tcp_user_timeout@ parameters, which
  -- bound the time the library can stall on network problems. This includes
  -- cleanup code that runs with asynchronous exceptions masked and thus
  -- can't be interrupted, e.g. cancellation of a query interrupted by an
  -- exception (though when built against @libpq@ < 17, @connect_timeout@
  -- doesn't cover the delivery of the cancellation request).
  , csClientEncoding :: !(Maybe T.Text)
  -- ^ Client-side encoding. If set to 'Nothing', database encoding is used.
  , csRole :: !(Maybe (RawSQL ()))
  -- ^ A custom role to set with "SET ROLE".
  }
  deriving stock (Eq, Ord, Show)

-- | Default connection settings. Note that all strings sent to PostgreSQL by
-- the library are encoded as UTF-8, so don't alter client encoding unless you
-- know what you're doing.
defaultConnectionSettings :: ConnectionSettings
defaultConnectionSettings =
  ConnectionSettings
    { csConnInfo = T.empty
    , csClientEncoding = Just "UTF-8"
    , csRole = Nothing
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
  deriving stock (Eq, Ord, Show)

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
-- time 'Database.PostgreSQL.PQTypes.Class.withConnection' is called.
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
-- /Warning:/ the t'Connection' needs to be explicitly destroyed with
-- 'disconnect', otherwise there will be a resource leak.
connect :: ConnectionSettings -> IO Connection
connect ConnectionSettings {..} = mask $ \unmask -> do
  connPtr <- openConnection unmask $ T.encodeUtf8 csConnInfo
  (`onException` c_PQfinish connPtr) . unmask $ do
    F.forM_ csClientEncoding $ \enc -> do
      res <- BS.useAsCString (T.encodeUtf8 enc) (c_PQsetClientEncoding connPtr)
      when (res == -1) $
        throwLibPQError connPtr fname
    conn <- do
      preparedQueries <- newIORef S.empty
      pid <- c_PQbackendPid connPtr
      pure
        Connection
          { connPtr = connPtr
          , connBackendPid = BackendPid $ fromIntegral pid
          , connPreparedQueries = preparedQueries
          }
    F.forM_ csRole $ \role -> runQueryIO conn $ "SET ROLE " <> role
    pure conn
  where
    fname = "connect"

    openConnection :: (forall r. IO r -> IO r) -> BS.ByteString -> IO (Ptr PGconn)
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
      -- itself and left behind. Note that this is why the child needs to
      -- allocate the buffer with the connection string itself: a buffer
      -- allocated by the parent would be freed when the exception unwinds
      -- its stack, potentially while 'PQconnectdb' still reads it.
      connVar <- newEmptyTMVarIO
      runningVar <- newTVarIO True
      _ <- forkIO $ do
        conn <- BS.useAsCString conninfo c_PQconnectdb
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
        when (status /= c_CONNECTION_OK) $
          throwLibPQError conn fname
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
    withSQL sql $ \query params ->
      withParams params $ \n oids values lengths formats -> do
        res <- execQueryInterruptible connPtr $ do
          c_PQexecParams connPtr query n oids values lengths formats c_FORMAT_BINARY
        pure (fromIntegral n, res)

-- | Name of a prepared query.
newtype QueryName = QueryName T.Text
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString)

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
      hpqTypesError "runPreparedQueryIO: unnamed prepared query is not supported"
    withSQL sql $ \query params -> do
      withParams params $ \n oids values lengths formats -> do
        BS.useAsCString (T.encodeUtf8 queryName) $ \cname -> do
          preparedQueries <- readIORef connPreparedQueries
          when (queryName `S.notMember` preparedQueries) $ do
            -- Prepare the statement synchronously: the blocking call can't
            -- be interrupted by asynchronous exceptions and the statement is
            -- recorded before they can be delivered again, so
            -- connPreparedQueries can't go out of sync with the server (a
            -- statement that exists server side without being recorded would
            -- break the connection: preparing it again fails with
            -- duplicate_prepared_statement, aborting the active transaction,
            -- if any). Preparation is expected to be quick (though it can
            -- block, e.g. on locks held by DDL statements), so like with
            -- COMMIT/ROLLBACK of transactions, interruptibility is not worth
            -- the trouble.
            E.mask_ $ do
              res <- c_PQprepare connPtr cname query n oids
              when (res == nullPtr) $ do
                throwLibPQError connPtr "runPreparedQueryIO"
              (`E.finally` c_PQclear res) $ do
                st <- c_PQresultStatus res
                if st == c_PGRES_COMMAND_OK
                  then modifyIORef' connPreparedQueries $ S.insert queryName
                  else -- Let 'verifyResult' throw an appropriate error.
                    void $ verifyResult connPtr res
          res <- execQueryInterruptible connPtr $ do
            c_PQexecPrepared connPtr cname n values lengths formats c_FORMAT_BINARY
          pure (fromIntegral n, res)

-- | Shared implementation of 'runQueryIO' and 'runPreparedQueryIO'.
--
-- Execution is interruptible with asynchronous exceptions (assuming the
-- caller doesn't have them hard masked); an interrupted query is cancelled
-- server side (see 'execQueryInterruptible').
--
-- Any synchronous exception thrown during execution is wrapped in
-- t'DBException' with the query attached as context.
runQueryImpl
  :: (HasCallStack, IsSQL sql)
  => Connection
  -> sql
  -> IO (Int, ForeignPtr PGresult)
  -- ^ Execute the query (see 'execQueryInterruptible') and return the number
  -- of its parameters along with the result.
  -> IO (Int, ForeignPtr PGresult, ConnectionStats -> ConnectionStats)
runQueryImpl Connection {..} sql execQuery = attachQueryContext $ do
  t1 <- getMonotonicTime
  (paramCount, res) <- execQuery
  t2 <- getMonotonicTime
  affected <- withForeignPtr res $ verifyResult connPtr
  -- Commands return no rows, so they contribute nothing to the row and
  -- value counts.
  (rows, columns) <- case affected of
    Left _ -> pure (0, 0)
    Right rows -> do
      columns <- fromIntegral <$> withForeignPtr res c_PQnfields
      pure (rows, columns)
  let updateStats stats =
        ConnectionStats
          { statsQueries = statsQueries stats + 1
          , statsRows = statsRows stats + rows
          , statsValues = statsValues stats + (rows * columns)
          , statsParams = statsParams stats + paramCount
          , statsTime = statsTime stats + (t2 - t1)
          }
  pure (either id id affected, res, updateStats)
  where
    -- Attach the query and the backend pid as context to exceptions thrown
    -- during execution.
    attachQueryContext m = m `E.catch` rethrowWithContext sql connBackendPid

----------------------------------------
-- Helpers

-- | Run a single query to completion, interruptibly when possible, and
-- return its result.
--
-- The blocking libpq call cannot be interrupted with asynchronous
-- exceptions, so it runs in a child thread and the parent waits for the
-- result, which keeps the wait interruptible (in the threaded runtime; in
-- the non-threaded one a blocking safe FFI call stalls the whole program).
-- An interrupted parent requests cancellation of the query, but still
-- waits for the child to finish: the child reads the query and its
-- parameters from buffers that are only valid until the parent unwinds out
-- of 'withParams', and once the exception propagates, the connection can
-- be handed back to its source (and e.g. closed) while the child is still
-- using it. Waiting also leaves the connection idle, ready to run queries
-- again.
--
-- If asynchronous exceptions are masked uninterruptibly, the wait couldn't
-- be interrupted anyway, so the blocking call is made directly in the
-- current thread.
execQueryInterruptible
  :: Ptr PGconn
  -> IO (Ptr PGresult)
  -- ^ The blocking libpq call executing the query.
  -> IO (ForeignPtr PGresult)
execQueryInterruptible connPtr execQuery =
  E.getMaskingState >>= \case
    E.MaskedUninterruptible -> checkResult =<< execQuery
    _ -> E.mask $ \restore -> do
      resVar <- newEmptyTMVarIO
      -- The child inherits the masked state, so the handover of the result
      -- cannot be interrupted.
      _ <- forkIO $ do
        res <- execQuery
        atomically $ putTMVar resVar res
      -- The result is read, not taken: nothing guarantees an asynchronous
      -- exception cannot arrive between the transaction commit and 'restore'
      -- re-masking (the current RTS has no delivery point there, but that's
      -- not part of the documented semantics), and then 'cancelQuery' has to
      -- find the result in place.
      rawRes <- restore (atomically $ readTMVar resVar) `E.onException` cancelQuery resVar
      checkResult rawRes
  where
    fname = "execQueryInterruptible"

    -- Wrap the result for GC and verify that the query could be sent and
    -- didn't put the connection in a copy mode.
    checkResult :: Ptr PGresult -> IO (ForeignPtr PGresult)
    checkResult rawRes = do
      when (rawRes == nullPtr) $ throwLibPQError connPtr fname
      res <- newForeignPtr c_ptr_PQclear rawRes
      st <- withForeignPtr res c_PQresultStatus
      -- The library doesn't support the copy modes a COPY statement puts the
      -- connection in. Erroring out is fine: libpq terminates the copy mode
      -- internally when the next query is executed.
      when (isCopyStatus st) $ do
        hpqTypesError $ fname ++ ": COPY statements are not supported"
      pure res

    -- Check whether a result status indicates one of the copy modes.
    isCopyStatus :: ExecStatusType -> Bool
    isCopyStatus st =
      st == c_PGRES_COPY_IN || st == c_PGRES_COPY_OUT || st == c_PGRES_COPY_BOTH

    -- Request cancellation of the query and wait until the child delivers
    -- the (discarded) result.
    cancelQuery :: TMVar (Ptr PGresult) -> IO ()
    cancelQuery resVar = E.uninterruptibleMask_ $ do
      -- If the query is already over, just release the result: a
      -- cancellation request would target no query in particular and merely
      -- waste a round trip to the server (it wouldn't affect subsequent
      -- queries though, as the backend discards cancellation requests
      -- received while idle).
      atomically (tryTakeTMVar resVar) >>= \case
        Just res -> c_PQclear res
        Nothing -> do
          requestCancellation
          awaitResult initialCancelDelay
      where
        -- Ask the server to cancel the query, ignoring errors (necessarily
        -- synchronous inside uninterruptibleMask): if the connection is
        -- broken, the blocking call fails as well and the child delivers an
        -- error result shortly anyway.
        requestCancellation :: IO ()
        requestCancellation =
          void (c_PQcancel connPtr) `E.catch` \(_ :: E.SomeException) -> pure ()

        -- Wait for the child to deliver the result, repeating the
        -- cancellation request with a progressively larger delay until it
        -- does.
        --
        -- A request that arrives before the backend started executing the
        -- query is silently discarded (cancellation is only acted upon
        -- while a command is running) and the only sign that one took
        -- effect is the query ending early, hence the repetition. The delay
        -- grows, as what matters here is how long the backend needs to read
        -- the query, which depends on its size and on how the server is
        -- reached. A request received during execution is remembered until
        -- the query reaches an interrupt check, so late repeats are merely
        -- wasted round trips and the growing delay keeps them rare.
        awaitResult :: Int -> IO ()
        awaitResult delay =
          waitResultFor delay >>= \case
            Just res -> c_PQclear res
            Nothing -> do
              requestCancellation
              awaitResult . min maxCancelDelay $ 2 * delay

        -- Wait until the child delivers the result or the given number of
        -- microseconds elapses ('System.Timeout.timeout' is of no use with
        -- asynchronous exceptions masked uninterruptibly).
        waitResultFor :: Int -> IO (Maybe (Ptr PGresult))
        waitResultFor micros = do
          expiredVar <- registerDelay micros
          atomically $
            (Just <$> takeTMVar resVar)
              `orElse` (Nothing <$ (check =<< readTVar expiredVar))

        initialCancelDelay :: Int
        initialCancelDelay = 50 * 1000 -- 50 ms
        maxCancelDelay :: Int
        maxCancelDelay = 60 * 1000 * 1000 -- 60 s

-- | Pass query parameters to the continuation in the format expected by
-- 'c_PQexecParams' and 'c_PQexecPrepared', i.e. the number of parameters
-- and arrays of their types, values, lengths and formats.
--
-- Note that pointers to the values alias the buffers of their ByteStrings
-- without copying, so they're valid only within the corresponding
-- 'BS.unsafeUseAsCStringLen' callback. This is why the parameters are
-- marshalled with 'withMany': the rest of the computation (including the
-- call to the continuation, which is where the pointers are read) runs
-- inside the callbacks of all the parameters.
withParams
  :: [PQParam]
  -> (CInt -> Ptr Oid -> Ptr CString -> Ptr CInt -> Ptr Format -> IO r)
  -> IO r
withParams params action =
  withMany withParam params $ \entries -> do
    let (oids, values, lengths) = unzip3 entries
    withArray oids $ \oidsPtr ->
      withArray values $ \valuesPtr ->
        withArray lengths $ \lengthsPtr ->
          withArray (replicate n c_FORMAT_BINARY) $ \formatsPtr ->
            action (fromIntegral n) oidsPtr valuesPtr lengthsPtr formatsPtr
  where
    n = length params

    -- Maximum size of a single value: MaxAllocSize from the PostgreSQL
    -- sources.
    maxValueSize :: Int
    maxValueSize = 0x3fffffff

    -- Marshal one parameter into its (oid, value, length) entry.
    withParam :: PQParam -> ((Oid, CString, CInt) -> IO r) -> IO r
    withParam (PQParam oid mvalue) k = case mvalue of
      Nothing -> k (oid, nullPtr, 0)
      Just value -> do
        -- Values larger than that cannot be stored by PostgreSQL, so reject
        -- them client side with a clear error. In particular, this ensures
        -- that the length fits in the CInt passed to libpq: a wrapped-around
        -- length would result in a confusing error or silent truncation of
        -- the value.
        when (BS.length value > maxValueSize) . hpqTypesError $
          "withParams: value of length "
            ++ show (BS.length value)
            ++ " is larger than the maximum size of a value ("
            ++ show maxValueSize
            ++ " bytes)"
        BS.unsafeUseAsCStringLen value $ \(ptr, len) ->
          if ptr == nullPtr
            -- A ByteString can be backed by a null pointer, which libpq would
            -- interpret as SQL NULL, so pass a non-null empty string instead.
            then k (oid, nullStringPtr, 0)
            else k (oid, ptr, fromIntegral len)

verifyResult
  :: Ptr PGconn
  -> Ptr PGresult
  -> IO (Either Int Int)
verifyResult conn res = do
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
    throwSQLError
      | res == nullPtr = throwLibPQError conn "verifyResult"
      | otherwise =
          E.throwIO
            =<< DetailedQueryError
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
      where
        field f = fromMaybe "" <$> mfield f
        mfield f = safePeekCString =<< c_PQresultErrorField res f

    throwParseError sn =
      hpqTypesError $
        "verifyResult: string returned by PQcmdTuples is not a valid number: " ++ show sn
