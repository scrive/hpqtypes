-- | Definition of internal DBT state.
module Database.PostgreSQL.PQTypes.Internal.State
  ( -- * ConnectionData
    ConnectionData
  , getConnectionSource
  , getConnectionAcquisitionModeIO
  , withConnectionData
  , changeAcquisitionModeTo
  , withConnection

    -- * DBState
  , DBState (..)
  , mkDBState
  , updateStateWith
  ) where

import Control.Concurrent qualified as C
import Control.Concurrent.MVar.Lifted
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Data.Function
import Data.IORef
import Data.Typeable
import Foreign.ForeignPtr
import GHC.Stack

import Data.Monoid.Utils
import Database.PostgreSQL.PQTypes.FromRow
import Database.PostgreSQL.PQTypes.Internal.BackendPid
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Connection
import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.Internal.Exception
import Database.PostgreSQL.PQTypes.Internal.QueryResult
import Database.PostgreSQL.PQTypes.Internal.Utils
import Database.PostgreSQL.PQTypes.SQL
import Database.PostgreSQL.PQTypes.SQL.Class
import Database.PostgreSQL.PQTypes.Transaction.Settings

data ConnectionState cdata
  = OnDemand
  | Acquired !IsolationLevel !Permissions !Connection !cdata
  | Finalized

-- Note: initConnection{State,Data} and finalizeConnection{State,Data} need to
-- be invoked inside bracket and run with asynchronous exceptions softly
-- masked. In addition, they may run queries that start/finish a
-- transaction. Running queries is a blocking (and thus interruptible)
-- operation, but if these queries are interrupted with an asynchronous
-- exception, then a connection is leaked, so they need to be run with
-- asynchronous exceptions hard masked with uninterruptibleMask.
--
-- What is more, these queries themselves can throw an exception if e.g. network
-- goes bye-bye and PostgreSQL can't be reached, therefore they need exception
-- handlers themselves so connections don't leak.

initConnectionState
  :: (MonadBase IO m, MonadMask m)
  => InternalConnectionSource m cdata
  -> ConnectionAcquisitionMode
  -> m (ConnectionState cdata)
initConnectionState ics = \case
  AcquireOnDemand -> pure OnDemand
  AcquireAndHold tsIsolationLevel tsPermissions -> do
    let initSql =
          smconcat
            [ "BEGIN"
            , case tsIsolationLevel of
                DefaultLevel -> ""
                ReadCommitted -> "ISOLATION LEVEL READ COMMITTED"
                RepeatableRead -> "ISOLATION LEVEL REPEATABLE READ"
                Serializable -> "ISOLATION LEVEL SERIALIZABLE"
            , case tsPermissions of
                DefaultPermissions -> ""
                ReadOnly -> "READ ONLY"
                ReadWrite -> "READ WRITE"
            ]
    (conn, cdata) <- takeConnection ics
    _ <- uninterruptibleMask_ $ do
      liftBase (runQueryIO @SQL conn initSql) `catch` \e -> do
        putConnection ics (conn, cdata) (ExitCaseException e)
        throwM e
    pure $ Acquired tsIsolationLevel tsPermissions conn cdata

finalizeConnectionState
  :: (HasCallStack, MonadBase IO m, MonadMask m)
  => InternalConnectionSource m cdata
  -> ExitCase r
  -> ConnectionState cdata
  -> m ()
finalizeConnectionState ics ec = \case
  OnDemand -> pure ()
  Acquired _ _ conn cdata -> do
    let finalizeSql = case ec of
          ExitCaseSuccess _ -> "COMMIT"
          _ -> "ROLLBACK"
    _ <- uninterruptibleMask_ $ do
      liftBase (runQueryIO @SQL conn finalizeSql) `catch` \e -> do
        putConnection ics (conn, cdata) (ExitCaseException e)
        throwM e
    putConnection ics (conn, cdata) ec
  Finalized -> error "finalized connection"

----------------------------------------

data ConnectionData m = forall cdata. ConnectionData
  { cdConnectionSource :: !(InternalConnectionSource m cdata)
  , cdConnectionState :: !(MVar (ConnectionState cdata))
  , cdAcquisitionMode :: !(IORef ConnectionAcquisitionMode)
  -- ^ Kept apart from the connection state, so that
  -- 'Database.PostgreSQL.PQTypes.Class.withNewSession' of a child thread
  -- does not wait for a query of the parent and works after the session ended.
  , cdOwnerThread :: !C.ThreadId
  -- ^ Thread that started the session. Only this thread can use it.
  }

getConnectionSource :: ConnectionData m -> ConnectionSourceM m
getConnectionSource ConnectionData {..} = ConnectionSourceM cdConnectionSource

getConnectionAcquisitionModeIO
  :: HasCallStack
  => ConnectionData m
  -> IO ConnectionAcquisitionMode
getConnectionAcquisitionModeIO ConnectionData {..} = readIORef cdAcquisitionMode

withConnectionData
  :: (HasCallStack, MonadBase IO m, MonadMask m)
  => ConnectionSourceM m
  -> TransactionSettings
  -> (ConnectionData m -> m r)
  -> m r
withConnectionData cs ts action = (`fix` 1) $ \loop n -> do
  eres <-
    try
      . fmap fst
      . generalBracket
        (initConnectionData cs cam)
        (\cd ec -> runCleanup ec $ finalizeConnectionData cd ec)
      $ action
  case eres of
    Right res -> pure res
    -- The restart happens outside of the exception handler, so that the
    -- retried transaction doesn't run with asynchronous exceptions masked.
    -- An asynchronous exception never triggers a restart. The restart
    -- predicate can match it if it's instantiated at SomeException.
    Left e
      | isAsyncException e -> throwM e
      | Just () <- expred n e -> loop $ n + 1
      | otherwise -> throwM e
  where
    cam = tsConnectionAcquisitionMode ts

    expred :: Integer -> SomeException -> Maybe ()
    expred n e = do
      -- check if the predicate exists
      RestartPredicate f <- tsRestartPredicate ts
      -- cast exception to the type expected by the predicate
      err <-
        msum
          [ -- either cast the exception itself...
            fromException e
          , -- ...or extract it from DBException
            fromException e >>= \DBException {..} -> cast dbeError
          ]
      -- check if the predicate allows for the restart
      guard $ f err n

changeAcquisitionModeTo
  :: (HasCallStack, MonadBase IO m, MonadMask m)
  => ConnectionAcquisitionMode
  -> DBState m
  -> m ()
changeAcquisitionModeTo cam st@DBState {dbConnectionData = ConnectionData {..}} = mask_ $ do
  checkSessionOwner st
  -- Each branch of 'mkNewState' determines the new connection state along with
  -- a follow-up action. The new state is installed before the follow-up action
  -- runs, because the action can throw, e.g. to rethrow a failed COMMIT.
  connState <- takeMVar cdConnectionState
  (newConnState, after) <-
    mkNewState connState `onException` putMVar cdConnectionState connState
  putMVar cdConnectionState newConnState
  liftBase $ atomicWriteIORef cdAcquisitionMode cam
  after
  where
    mkNewState = \case
      OnDemand -> case cam of
        AcquireOnDemand -> pure (OnDemand, pure ())
        AcquireAndHold {} -> do
          -- If initConnectionState throws, no connection is held, so the
          -- state stays OnDemand (restored by the onException handler above).
          newConnState <- initConnectionState cdConnectionSource cam
          pure (newConnState, pure ())
      connState@(Acquired isolationLevel permissions _ _) -> case cam of
        AcquireOnDemand -> do
          -- If finalizeConnectionState throws (e.g. COMMIT fails), it has
          -- already returned the connection to the source, so the state needs
          -- to become OnDemand either way, otherwise it would refer to an
          -- invalid connection.
          eres :: Either SomeException () <- try $ do
            finalizeConnectionState cdConnectionSource (ExitCaseSuccess ()) connState
          pure (OnDemand, either throwM pure eres)
        AcquireAndHold newIsolationLevel newPermissions -> do
          when (isolationLevel /= newIsolationLevel) $ do
            error $
              "isolation level mismatch (current: "
                ++ show isolationLevel
                ++ ", new: "
                ++ show newIsolationLevel
                ++ ")"
          when (permissions /= newPermissions) $ do
            error $
              "permissions mismatch (current: "
                ++ show permissions
                ++ ", new: "
                ++ show newPermissions
                ++ ")"
          pure (connState, pure ())
      Finalized -> error "finalized connection"

withConnection
  :: (HasCallStack, MonadBase IO m, MonadMask m)
  => DBState m
  -> (Connection -> m r)
  -> m r
withConnection st@DBState {dbConnectionData = ConnectionData {..}} action = do
  checkSessionOwner st
  bracket (takeMVar cdConnectionState) (putMVar cdConnectionState) $ \case
    OnDemand ->
      fst
        <$> generalBracket
          (takeConnection cdConnectionSource)
          (putConnection cdConnectionSource)
          ( \(conn, _cdata) ->
              fmap fst
                . generalBracket
                  (autoQuery conn "BEGIN READ ONLY")
                  -- If the ROLLBACK fails after the action failed, the
                  -- connection returns to its source as failed either way,
                  -- and the source disposes of it.
                  (\() ec -> runCleanup ec $ autoQuery conn "ROLLBACK")
                $ \() -> action conn
          )
    Acquired _ _ conn _ -> action conn
    Finalized -> error "finalized connection"
  where
    -- The queries that delimit the automatic transaction can't be
    -- interrupted, otherwise the connection would end up in an unexpected
    -- transaction state.
    autoQuery :: MonadBase IO m => Connection -> SQL -> m ()
    autoQuery conn sql =
      liftBase . uninterruptibleMask_ . void $ runQueryIO @SQL conn sql

checkSessionOwner
  :: (HasCallStack, MonadBase IO m, MonadThrow m)
  => DBState m
  -> m ()
checkSessionOwner DBState {dbConnectionData = ConnectionData {..}, ..} = do
  currentThread <- liftBase C.myThreadId
  when (currentThread /= cdOwnerThread) $ do
    case dbLastQuery of
      (pid, SomeSQL sql) ->
        throwM
          DBException
            { dbeQueryContext = sql
            , dbeBackendPid = pid
            , dbeError =
                ThreadMismatchError
                  { tmeOwnerThread = cdOwnerThread
                  , tmeCurrentThread = currentThread
                  }
            , dbeCallStack = callStack
            }

initConnectionData
  :: (MonadBase IO m, MonadMask m)
  => ConnectionSourceM m
  -> ConnectionAcquisitionMode
  -> m (ConnectionData m)
initConnectionData (ConnectionSourceM ics) cam = do
  ownerThread <- liftBase C.myThreadId
  connState <- newMVar =<< initConnectionState ics cam
  acquisitionMode <- liftBase $ newIORef cam
  pure $
    ConnectionData
      { cdConnectionSource = ics
      , cdConnectionState = connState
      , cdAcquisitionMode = acquisitionMode
      , cdOwnerThread = ownerThread
      }

finalizeConnectionData
  :: (HasCallStack, MonadBase IO m, MonadMask m)
  => ConnectionData m
  -> ExitCase r
  -> m ()
finalizeConnectionData ConnectionData {..} ec = do
  connState <- takeMVar cdConnectionState
  finalizeConnectionState cdConnectionSource ec connState
    `finally` putMVar cdConnectionState Finalized

----------------------------------------

-- | Internal DB state.
data DBState m = DBState
  { dbConnectionData :: !(ConnectionData m)
  -- ^ Active connection.
  , dbConnectionStats :: !ConnectionStats
  -- ^ Statistics associated with the session.
  , dbRestartPredicate :: !(Maybe RestartPredicate)
  -- ^ Restart predicate from initial 'TransactionSettings'.
  , dbLastQuery :: !(BackendPid, SomeSQL)
  -- ^ Last SQL query that was executed along with ID of the server process
  -- attached to the session that executed it.
  , dbRecordLastQuery :: !Bool
  -- ^ Whether running query should override 'dbLastQuery'.
  , dbQueryResult :: !(forall row. FromRow row => Maybe (QueryResult row))
  -- ^ Current query result.
  }

mkDBState
  :: ConnectionData m
  -> TransactionSettings
  -> DBState m
mkDBState cd ts =
  DBState
    { dbConnectionData = cd
    , dbConnectionStats = initialConnectionStats
    , dbRestartPredicate = tsRestartPredicate ts
    , dbLastQuery = (noBackendPid, SomeSQL (mempty :: SQL))
    , dbRecordLastQuery = True
    , dbQueryResult = Nothing
    }

updateStateWith
  :: IsSQL sql
  => Connection
  -> DBState m
  -> sql
  -> (r, ForeignPtr PGresult, ConnectionStats -> ConnectionStats)
  -> IO (r, DBState m)
updateStateWith conn st sql (r, res, updateStats) = do
  pure
    ( r
    , st
        { dbConnectionStats = updateStats $ dbConnectionStats st
        , dbLastQuery =
            if dbRecordLastQuery st
              then (connBackendPid conn, SomeSQL sql)
              else dbLastQuery st
        , dbQueryResult = Just $ mkQueryResult sql (connBackendPid conn) res
        }
    )
