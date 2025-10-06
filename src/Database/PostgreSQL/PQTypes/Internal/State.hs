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

import Control.Concurrent.MVar.Lifted
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Data.Function
import Data.Typeable
import Foreign.ForeignPtr
import GHC.Stack

import Data.Monoid.Utils
import Database.PostgreSQL.PQTypes.FromRow
import Database.PostgreSQL.PQTypes.Internal.BackendPid
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Connection
import Database.PostgreSQL.PQTypes.Internal.Exception
import Database.PostgreSQL.PQTypes.Internal.QueryResult
import Database.PostgreSQL.PQTypes.SQL
import Database.PostgreSQL.PQTypes.SQL.Class
import Database.PostgreSQL.PQTypes.Transaction.Settings

data ConnectionState cdata
  = OnDemand
  | Acquired !IsolationLevel !Permissions !Connection !cdata
  | Finalized

-- Note: initConnectionState and finalizeConnectionState are invoked inside
-- bracket and run with asynchronous exceptions softly masked. However, both of
-- them may run queries that start/finish a transaction. Running queries is a
-- blocking (and thus interruptible) operation, but if these queries are
-- interrupted with an asynchronous exception, then a connection is leaked, so
-- they need to be run with asynchronous exceptions hard masked with
-- uninterruptibleMask.

initConnectionState
  :: MonadBase IO m
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
    _ <- liftBase . uninterruptibleMask_ $ runQueryIO @SQL conn initSql
    pure $ Acquired tsIsolationLevel tsPermissions conn cdata

finalizeConnectionState
  :: (HasCallStack, MonadBase IO m)
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
    _ <- liftBase . uninterruptibleMask_ $ runQueryIO @SQL conn finalizeSql
    putConnection ics (conn, cdata) ec
  Finalized -> error "finalized connection"

----------------------------------------

data ConnectionData m = forall cdata. ConnectionData
  { cdConnectionSource :: !(InternalConnectionSource m cdata)
  , cdConnectionState :: !(MVar (ConnectionState cdata))
  }

getConnectionSource :: ConnectionData m -> ConnectionSourceM m
getConnectionSource ConnectionData {..} = ConnectionSourceM cdConnectionSource

getConnectionAcquisitionModeIO
  :: HasCallStack
  => ConnectionData m
  -> IO ConnectionAcquisitionMode
getConnectionAcquisitionModeIO ConnectionData {..} = do
  readMVar cdConnectionState >>= \case
    OnDemand -> pure AcquireOnDemand
    Acquired isolationLevel permissions _ _ -> do
      pure $ AcquireAndHold isolationLevel permissions
    Finalized -> error "finalized connectio"

withConnectionData
  :: (HasCallStack, MonadBase IO m, MonadMask m)
  => ConnectionSourceM m
  -> TransactionSettings
  -> (ConnectionData m -> m r)
  -> m r
withConnectionData cs ts action = (`fix` 1) $ \loop n -> do
  let maybeRestart = case tsRestartPredicate ts of
        Just _ -> handleJust (expred n) $ \_ -> loop $ n + 1
        Nothing -> id
  maybeRestart
    . fmap fst
    . generalBracket (initConnectionData cs cam) finalizeConnectionData
    $ action
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
  -> ConnectionData m
  -> m ()
changeAcquisitionModeTo cam ConnectionData {..} = mask_ $ do
  bracketOnError (takeMVar cdConnectionState) (putMVar cdConnectionState) $ \case
    OnDemand -> case cam of
      AcquireOnDemand -> putMVar cdConnectionState OnDemand
      _ -> do
        newConnState <- initConnectionState cdConnectionSource cam
        putMVar cdConnectionState newConnState
    connState@(Acquired isolationLevel permissions _ _) -> case cam of
      AcquireOnDemand -> do
        finalizeConnectionState cdConnectionSource (ExitCaseSuccess ()) connState
        putMVar cdConnectionState OnDemand
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
        putMVar cdConnectionState connState
    Finalized -> error "finalized connection"

withConnection
  :: (HasCallStack, MonadBase IO m, MonadMask m)
  => ConnectionData m
  -> (Connection -> m r)
  -> m r
withConnection ConnectionData {..} action = do
  bracket (takeMVar cdConnectionState) (putMVar cdConnectionState) $ \case
    OnDemand ->
      fst
        <$> generalBracket
          (takeConnection cdConnectionSource)
          (putConnection cdConnectionSource)
          ( \(conn, _cdata) ->
              bracket_
                (liftBase . uninterruptibleMask_ $ runQueryIO @SQL conn "BEGIN READ ONLY")
                (liftBase . uninterruptibleMask_ $ runQueryIO @SQL conn "ROLLBACK")
                (action conn)
          )
    Acquired _ _ conn _ -> action conn
    Finalized -> error "finalized connection"

initConnectionData
  :: MonadBase IO m
  => ConnectionSourceM m
  -> ConnectionAcquisitionMode
  -> m (ConnectionData m)
initConnectionData (ConnectionSourceM ics) cam = do
  connState <- newMVar =<< initConnectionState ics cam
  pure $
    ConnectionData
      { cdConnectionSource = ics
      , cdConnectionState = connState
      }

finalizeConnectionData
  :: (HasCallStack, MonadBase IO m, MonadMask m)
  => ConnectionData m
  -> ExitCase r
  -> m ()
finalizeConnectionData ConnectionData {..} ec = do
  (`finally` putMVar cdConnectionState Finalized) $ do
    connState <- takeMVar cdConnectionState
    finalizeConnectionState cdConnectionSource ec connState

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
