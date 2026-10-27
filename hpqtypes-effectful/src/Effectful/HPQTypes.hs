{-# OPTIONS_GHC -Wno-orphans #-}

-- | Access to a PostgreSQL database via 'MonadDB'.
module Effectful.HPQTypes
  ( -- * Effect
    DB (..)

    -- ** Handlers
  , runDB

    -- * Re-exports
  , module Database.PostgreSQL.PQTypes
  ) where

import Database.PostgreSQL.PQTypes
import Database.PostgreSQL.PQTypes.Internal.Connection qualified as PQ
import Database.PostgreSQL.PQTypes.Internal.Notification qualified as PQ
import Database.PostgreSQL.PQTypes.Internal.State qualified as PQ
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Exception
import Effectful.State.Static.Local qualified as State
import GHC.Stack

-- | Provide the ability to access a PostgreSQL database via 'MonadDB'.
data DB :: Effect where
  RunQuery :: IsSQL sql => sql -> DB m Int
  RunPreparedQuery :: IsSQL sql => PQ.QueryName -> sql -> DB m Int
  GetLastQuery :: DB m (BackendPid, SomeSQL)
  WithFrozenLastQuery :: m a -> DB m a
  GetConnectionStats :: DB m PQ.ConnectionStats
  GetQueryResult :: FromRow row => DB m (Maybe (QueryResult row))
  ClearQueryResult :: DB m ()
  GetConnectionAcquisitionMode :: DB m ConnectionAcquisitionMode
  AcquireAndHoldConnection :: IsolationLevel -> Permissions -> DB m ()
  UnsafeAcquireOnDemandConnection :: DB m ()
  GetNotification :: Int -> DB m (Maybe PQ.Notification)
  WithNewSession :: m a -> DB m a

type instance DispatchOf DB = Dynamic

-- | Orphan, canonical instance.
instance DB :> es => MonadDB (Eff es) where
  runQuery = withFrozenCallStack $ send . RunQuery
  runPreparedQuery name = withFrozenCallStack $ send . RunPreparedQuery name
  getLastQuery = send GetLastQuery
  withFrozenLastQuery = send . WithFrozenLastQuery
  getConnectionStats = withFrozenCallStack $ send GetConnectionStats
  getQueryResult = send GetQueryResult
  clearQueryResult = send ClearQueryResult
  getConnectionAcquisitionMode = send GetConnectionAcquisitionMode
  acquireAndHoldConnection isoLevel = send . AcquireAndHoldConnection isoLevel
  unsafeAcquireOnDemandConnection = send UnsafeAcquireOnDemandConnection
  getNotification = send . GetNotification
  withNewSession = send . WithNewSession

-- | Run the 'DB' effect with the given connection source and transaction
-- settings.
--
-- The session is bound to the calling thread. If another thread invokes a
-- 'MonadDB' operation that uses the connection, the operation throws
-- 'ThreadMismatchError' wrapped in 'DBException'. To run queries from another
-- thread, start a separate session there with 'withNewSession'.
--
-- /Note:/ this is the @effectful@ version of 'runDBT'.
runDB
  :: IOE :> es
  => PQ.ConnectionSourceM (Eff es)
  -- ^ Connection source.
  -> TransactionSettings
  -- ^ Transaction settings.
  -> Eff (DB : es) a
  -> Eff es a
runDB cs0 ts0 m = PQ.withConnectionData cs0 ts0 $ \cd0 -> do
  reinterpretWith (State.evalState $ PQ.mkDBState cd0 ts0) m $ \env -> \case
    RunQuery sql -> modifyState $ \st -> withFrozenCallStack $ do
      PQ.withConnection st $ \conn -> do
        liftIO $ PQ.updateStateWith conn st sql =<< PQ.runQueryIO conn sql
    RunPreparedQuery name sql -> modifyState $ \st -> withFrozenCallStack $ do
      PQ.withConnection st $ \conn -> do
        liftIO $ PQ.updateStateWith conn st sql =<< PQ.runPreparedQueryIO conn name sql
    GetLastQuery -> PQ.dbLastQuery <$> get
    WithFrozenLastQuery action -> do
      origValue <- PQ.dbRecordLastQuery <$> get
      bracket_
        (modify $ \st -> st {PQ.dbRecordLastQuery = False})
        (modify $ \st -> st {PQ.dbRecordLastQuery = origValue})
        (localSeqUnlift env $ \unlift -> unlift action)
    GetConnectionStats -> PQ.dbConnectionStats <$> get
    GetQueryResult -> PQ.dbQueryResult <$> get
    ClearQueryResult -> modify $ \st -> st {PQ.dbQueryResult = Nothing}
    GetConnectionAcquisitionMode -> do
      liftIO . PQ.getConnectionAcquisitionModeIO . PQ.dbConnectionData =<< get
    AcquireAndHoldConnection isolationLevel permissions -> withState $ \st -> do
      PQ.changeAcquisitionModeTo (AcquireAndHold isolationLevel permissions) st
    UnsafeAcquireOnDemandConnection -> withState $ \st -> do
      PQ.changeAcquisitionModeTo AcquireOnDemand st
    GetNotification time -> withState $ \st -> do
      PQ.withConnection st $ \conn -> do
        liftIO $ PQ.getNotificationIO conn time
    WithNewSession action -> do
      st <- get
      cam <- liftIO . PQ.getConnectionAcquisitionModeIO $ PQ.dbConnectionData st
      let cs = PQ.getConnectionSource $ PQ.dbConnectionData st
          ts =
            TransactionSettings
              { tsRestartPredicate = PQ.dbRestartPredicate st
              , tsConnectionAcquisitionMode = cam
              }
      raiseWith SeqUnlift $ \lower -> do
        PQ.withConnectionData cs ts $ \cd -> lower $ do
          localSeqUnlift env $ \unlift -> do
            bracket_ (put $ PQ.mkDBState cd ts) (put st) $ unlift action

----------------------------------------
-- Internal helpers

-- | Internal state used to reinterpret the `DB` effect
type DBState es = PQ.DBState (Eff es)

get :: Eff (State.State (DBState es) : es) (DBState es)
get = State.get

put :: DBState es -> Eff (State.State (DBState es) : es) ()
put = State.put

modify :: (DBState es -> DBState es) -> Eff (State.State (DBState es) : es) ()
modify = State.modify

modifyState
  :: (DBState es -> Eff es (a, DBState es))
  -> Eff (State.State (DBState es) : es) a
modifyState action = do
  s0 <- get
  (a, s) <- raise $ action s0
  put s
  pure a

withState
  :: (DBState es -> Eff es a)
  -> Eff (State.State (DBState es) : es) a
withState action = raise . action =<< get
