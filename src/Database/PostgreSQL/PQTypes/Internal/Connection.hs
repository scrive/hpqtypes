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
import Foreign.Ptr
import GHC.Clock (getMonotonicTime)
import GHC.Conc (closeFdWith)
import GHC.Stack
import System.Posix.Types

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
  connPtr <- openConnection unmask $ T.encodeUtf8 csConnInfo
  (`onException` c_PQfinish connPtr) . unmask $ do
    -- Sending queries doesn't rely on libpq blocking the whole OS thread while
    -- flushing the output; instead the runtime system waits for the socket to
    -- become ready, so put the connection in the non-blocking mode.
    nonblocking <- c_PQsetnonblocking connPtr 1
    when (nonblocking == -1) $
      throwLibPQError connPtr fname
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
        res <- sendQueryAndGetResult connPtr $ do
          c_PQsendQueryParams connPtr query n oids values lengths formats c_RESULT_BINARY
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
      E.throwIO
        DBException
          { dbeQueryContext = sql
          , dbeBackendPid = connBackendPid
          , dbeError = HPQTypesError "runPreparedQueryIO: unnamed prepared query is not supported"
          , dbeCallStack = callStack
          }
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
          res <- sendQueryAndGetResult connPtr $ do
            c_PQsendQueryPrepared connPtr cname n values lengths formats c_RESULT_BINARY
          pure (fromIntegral n, res)

-- | Shared implementation of 'runQueryIO' and 'runPreparedQueryIO'.
--
-- The query is executed using the asynchronous API of libpq: potentially
-- blocking operations wait for socket readiness using GHC's IO manager, so
-- (assuming the caller doesn't have them hard masked) execution is
-- interruptible with asynchronous exceptions without spawning any additional
-- threads. If an exception arrives while the query is in progress, its
-- cancellation is requested and the connection is drained of pending
-- results, so that it can be used to run queries again afterwards.
--
-- Any synchronous exception thrown during execution is wrapped in
-- 'DBException' with the query attached as context.
runQueryImpl
  :: (HasCallStack, IsSQL sql)
  => Connection
  -> sql
  -> IO (Int, ForeignPtr PGresult)
  -- ^ Execute the query (see 'sendQueryAndGetResult') and return the number
  -- of its parameters along with the result.
  -> IO (Int, ForeignPtr PGresult, ConnectionStats -> ConnectionStats)
runQueryImpl Connection {..} sql execQuery = E.mask $ \restore -> attachQueryContext $ do
  t1 <- getMonotonicTime
  -- If execution is interrupted by an exception (usually an asynchronous
  -- one, but 'E.throwTo' can also deliver synchronous exception types from
  -- other threads), the query might still be running server side and its
  -- results were not consumed, so the connection cannot be used to run
  -- queries as is; 'cancelQuery' puts it back into the idle state.
  (paramCount, res) <- restore execQuery `E.onException` cancelQuery
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

    cancelQuery = E.uninterruptibleMask_ $ do
      -- Do nothing unless a query is in progress, i.e. it was sent to the
      -- server and its results were not fully received (which libpq tracks
      -- client side). This is not the case when the exception arrived before
      -- anything was sent (e.g. during marshalling of the parameters) or
      -- after the result was fully received (e.g. when preparation of a
      -- prepared query failed): the cancellation request would then target
      -- no query in particular and merely waste a round trip to the server.
      -- Note that it wouldn't affect subsequent queries though: 'c_PQcancel'
      -- returns only once the server acknowledged the receipt of the request
      -- and the backend discards cancellation requests received while idle.
      status <- c_PQtransactionStatus connPtr
      when (status == c_PQTRANS_ACTIVE) $ do
        -- The part of the query that potentially sits in the output buffer
        -- needs to be flushed first, otherwise the server won't start (and
        -- thus won't finish) the query and draining the results below would
        -- deadlock. Errors are ignored as there's nothing to be done about
        -- them at this point; if the connection is broken, the next attempt
        -- to use it will fail and it will be disposed of by its connection
        -- source.
        ignoreErrors $ flushOutput connPtr
        -- Request cancellation of the query only if its results are not
        -- already sitting in the input buffer, i.e. the server might still
        -- be executing it. Note that the request cannot be sent before
        -- flushing: until the query is fully transmitted, no command is
        -- running, so the request would be ignored by the server.
        busy <- c_PQisBusy connPtr
        when (busy == 1) . void $ c_PQcancel connPtr
        -- Consume the results, so that the connection can be used again.
        ignoreErrors $ drainResults connPtr
      where
        -- Swallowing exceptions is fine here since we're inside
        -- uninterruptibleMask so the one we catch are necessarily synchronous.
        ignoreErrors m = m `E.catch` \(_ :: E.SomeException) -> pure ()

----------------------------------------
-- Helpers

-- | Run a single query: send it, flush the output buffer and receive the
-- result.
sendQueryAndGetResult
  :: Ptr PGconn
  -> IO CInt
  -- ^ Send the query for execution.
  -> IO (ForeignPtr PGresult)
sendQueryAndGetResult connPtr send = do
  sent <- send
  when (sent /= 1) $ throwLibPQError connPtr "sendQueryAndGetResult"
  flushOutput connPtr
  receiveResult connPtr

-- | Pass query parameters to the continuation in the format expected by
-- 'c_PQsendQueryParams' and 'c_PQsendQueryPrepared', i.e. the number of
-- parameters and arrays of their types, values, lengths and formats.
--
-- Note that pointers to the values alias the buffers of their ByteStrings
-- without copying, so they're valid only within the corresponding
-- 'BS.unsafeUseAsCStringLen' callback. This is why the processing is done in
-- the continuation passing style: the rest of the computation (including the
-- call to the continuation, which is where the pointers are read) needs to
-- run inside the callbacks of all the parameters.
withParams
  :: [PQParam]
  -> (CInt -> Ptr Oid -> Ptr CString -> Ptr CInt -> Ptr CInt -> IO r)
  -> IO r
withParams params action =
  foldr step base params $ \oids values lengths ->
    withArray oids $ \oidsPtr ->
      withArray values $ \valuesPtr ->
        withArray lengths $ \lengthsPtr ->
          withArray (replicate n 1) $ \formatsPtr ->
            action (fromIntegral n) oidsPtr valuesPtr lengthsPtr formatsPtr
  where
    n = length params

    base :: ([Oid] -> [CString] -> [CInt] -> IO r) -> IO r
    base k = k [] [] []

    -- Marshal one parameter: rest marshals the parameters after this one,
    -- i.e. given a continuation expecting the oids/values/lengths of those
    -- parameters, it calls it from inside their 'BS.unsafeUseAsCStringLen'
    -- callbacks. The continuation passed to rest prepends the entries of
    -- this parameter and is nested inside its own callback, so that the
    -- pointer stays valid until the continuation of the whole fold runs.
    -- For [p1, p2] the fold unrolls to
    --
    -- > BS.unsafeUseAsCStringLen v1 $ \(ptr1, len1) ->
    -- >   BS.unsafeUseAsCStringLen v2 $ \(ptr2, len2) ->
    -- >     k [oid1, oid2] [ptr1, ptr2] [len1, len2]
    --
    -- where k is the lambda given to the fold above.
    step
      :: PQParam
      -> (([Oid] -> [CString] -> [CInt] -> IO r) -> IO r)
      -> (([Oid] -> [CString] -> [CInt] -> IO r) -> IO r)
    step (PQParam oid mvalue) rest k = case mvalue of
      Nothing -> rest $ \oids values lengths ->
        k (oid : oids) (nullPtr : values) (0 : lengths)
      Just value -> BS.unsafeUseAsCStringLen value $ \(ptr, len) ->
        rest $ \oids values lengths ->
          if ptr == nullPtr
            -- A ByteString can be backed by a null pointer, which libpq would
            -- interpret as SQL NULL, so pass a non-null empty string instead.
            then k (oid : oids) (nullStringPtr : values) (0 : lengths)
            else k (oid : oids) (ptr : values) (fromIntegral len : lengths)

-- | Flush any data queued in the output buffer to the server, waiting for the
-- socket to become ready as necessary.
flushOutput :: Ptr PGconn -> IO ()
flushOutput connPtr =
  c_PQflush connPtr >>= \case
    0 -> pure ()
    1 -> do
      -- The output buffer is not empty. Wait until the socket becomes either
      -- write-ready (there's room for more data) or read-ready (the server
      -- might not read our data until we consume some of its output first),
      -- then consume the input and retry.
      fd <- getSocket connPtr
      (waitRead, dropWaitRead) <- threadWaitReadSTM fd
      (waitWrite, dropWaitWrite) <- threadWaitWriteSTM fd
      atomically (waitRead `orElse` waitWrite)
        `E.finally` (dropWaitRead >> dropWaitWrite)
      consumeInput connPtr
      flushOutput connPtr
    _ -> throwLibPQError connPtr "flushOutput"

-- | Receive the result of a query. 'c_PQgetResult' is called until it returns
-- NULL as required by libpq to put the connection back into the idle state; a
-- single query always produces exactly one result, but if there somehow is
-- more than one, the last one is kept, mirroring the behavior of PQexec.
receiveResult :: Ptr PGconn -> IO (ForeignPtr PGresult)
receiveResult connPtr = loop Nothing
  where
    loop acc = do
      waitForResult connPtr
      mres <- E.mask_ $ do
        res <- c_PQgetResult connPtr
        if res == nullPtr
          then pure Nothing
          else Just <$> newForeignPtr c_ptr_PQclear res
      case mres of
        Just res -> do
          -- In the copy modes every call to 'c_PQgetResult' produces a fresh
          -- result with the corresponding status instead of eventually
          -- returning NULL, so the loop would never terminate.
          st <- withForeignPtr res c_PQresultStatus
          when (isCopyStatus st) $
            hpqTypesError "receiveResult: COPY statements are not supported"
          loop $ Just res
        Nothing -> case acc of
          Just res -> pure res
          Nothing -> hpqTypesError "receiveResult: query produced no results"

-- | Wait until a call to 'c_PQgetResult' will not block.
waitForResult :: Ptr PGconn -> IO ()
waitForResult connPtr = do
  busy <- c_PQisBusy connPtr
  when (busy == 1) $ do
    fd <- getSocket connPtr
    threadWaitRead fd
    consumeInput connPtr
    waitForResult connPtr

-- | Consume the results of a cancelled query so that the connection can be
-- used to run queries again.
drainResults :: Ptr PGconn -> IO ()
drainResults connPtr = do
  waitForResult connPtr
  res <- c_PQgetResult connPtr
  when (res /= nullPtr) $ do
    st <- c_PQresultStatus res
    c_PQclear res
    -- In the copy modes every call to 'c_PQgetResult' produces a fresh
    -- result with the corresponding status instead of eventually returning
    -- NULL, so the loop would never terminate. Stop draining; the connection
    -- is unusable and will be disposed of by its connection source.
    unless (isCopyStatus st) $ drainResults connPtr

-- | Check whether a result status indicates that the connection entered one
-- of the copy modes due to execution of a COPY statement.
isCopyStatus :: ExecStatusType -> Bool
isCopyStatus st =
  st == c_PGRES_COPY_IN || st == c_PGRES_COPY_OUT || st == c_PGRES_COPY_BOTH

-- | Read the data that arrived from the server, erroring out on failure.
consumeInput :: Ptr PGconn -> IO ()
consumeInput connPtr = do
  res <- c_PQconsumeInput connPtr
  when (res /= 1) $ throwLibPQError connPtr "consumeInput"

-- | Get the file descriptor of the connection socket, erroring out if the
-- connection is lost.
getSocket :: Ptr PGconn -> IO Fd
getSocket connPtr = do
  fd <- c_PQsocket connPtr
  when (fd == -1) $ throwLibPQError connPtr "getSocket"
  pure fd

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
