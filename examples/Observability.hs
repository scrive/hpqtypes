{-# LANGUAGE InstanceSigs #-}
module Observability  where
import Database.PostgreSQL.PQTypes
import Database.PostgreSQL.PQTypes.Internal.Monad
import Control.Applicative
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad
import GHC.Stack

type InstrumentedDBT m = InstrumentedDBT_ m m

runInstrumentedDBT :: InstrumentedDBT m a -> DBT m a
runInstrumentedDBT = unInstrumentedDBT_

newtype InstrumentedDBT_ n m a = InstrumentedDBT_ {unInstrumentedDBT_ :: DBT_ n m a}
  deriving newtype
    ( Alternative
    , Applicative
    , Functor
    , Monad
    , MonadBase b
    , MonadThrow
    , MonadCatch
    , MonadMask
    , MonadFail
    , MonadIO
    , MonadPlus
    )

instance (Monad m, MonadIO m, Applicative m, MonadBase IO m, MonadMask m) => MonadDB (InstrumentedDBT m) where
  runQuery :: (HasCallStack, IsSQL sql) => sql -> InstrumentedDBT m Int
  runQuery sql = InstrumentedDBT_ . makeTrace sql $ runQuery sql

  runPreparedQuery :: (HasCallStack, IsSQL sql) => QueryName -> sql -> InstrumentedDBT m Int
  runPreparedQuery queryName sql = InstrumentedDBT_ . makeTrace sql $ runPreparedQuery queryName sql

  getLastQuery :: InstrumentedDBT m (BackendPid, SomeSQL)
  getLastQuery = InstrumentedDBT_ getLastQuery

  withFrozenLastQuery :: InstrumentedDBT m a -> InstrumentedDBT m a
  withFrozenLastQuery m = InstrumentedDBT_ {unInstrumentedDBT_ = withFrozenLastQuery $ unInstrumentedDBT_ m}

  getConnectionStats :: HasCallStack => InstrumentedDBT m ConnectionStats
  getConnectionStats = InstrumentedDBT_ getConnectionStats

  getQueryResult :: FromRow row => InstrumentedDBT m (Maybe (QueryResult row))
  getQueryResult = InstrumentedDBT_ getQueryResult

  clearQueryResult :: InstrumentedDBT m ()
  clearQueryResult = InstrumentedDBT_ clearQueryResult

  getNotification :: Int -> InstrumentedDBT m (Maybe Notification)
  getNotification = InstrumentedDBT_ . getNotification

  withNewConnection :: InstrumentedDBT m a -> InstrumentedDBT m a
  withNewConnection m = InstrumentedDBT_ . withNewConnection $ unInstrumentedDBT_ m

  getConnectionAcquisitionMode :: HasCallStack => InstrumentedDBT m ConnectionAcquisitionMode
  getConnectionAcquisitionMode = InstrumentedDBT_ getConnectionAcquisitionMode

  acquireAndHoldConnection :: HasCallStack => IsolationLevel -> Permissions -> InstrumentedDBT m ()
  acquireAndHoldConnection level perm = InstrumentedDBT_  $ acquireAndHoldConnection level perm

  unsafeAcquireOnDemandConnection :: HasCallStack => InstrumentedDBT m ()
  unsafeAcquireOnDemandConnection = InstrumentedDBT_  unsafeAcquireOnDemandConnection

makeTrace :: (MonadMask m, HasCallStack, Monad m, MonadIO m, IsSQL sql) => sql -> m a -> m a
makeTrace sql m = do
  let desc = sqlDescription sql
  ret <- m
  liftIO $ print desc
  pure ret
