module Database.PostgreSQL.PQTypes.Internal.Monad
  ( DBT_ (..)
  , DBT
  , runDBT
  , mapDBT
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Error.Class
import Control.Monad.Fail qualified as MF
import Control.Monad.Reader.Class
import Control.Monad.State.Strict
import Control.Monad.Trans.Control
import Control.Monad.Trans.State.Strict qualified as S
import Control.Monad.Writer.Class
import GHC.Stack

import Database.PostgreSQL.PQTypes.Class
import Database.PostgreSQL.PQTypes.Internal.Connection
import Database.PostgreSQL.PQTypes.Internal.Notification
import Database.PostgreSQL.PQTypes.Internal.State
import Database.PostgreSQL.PQTypes.Transaction.Settings

type InnerDBT m = StateT (DBState m)

-- | Monad transformer for adding database
-- interaction capabilities to the underlying monad.
newtype DBT_ m n a = DBT {unDBT :: InnerDBT m n a}
  deriving (Alternative, Applicative, Functor, Monad, MF.MonadFail, MonadBase b, MonadCatch, MonadIO, MonadMask, MonadPlus, MonadThrow, MonadTrans)

type DBT m = DBT_ m m

-- | Evaluate monadic action with supplied
-- connection source and transaction settings.
runDBT
  :: (HasCallStack, MonadBase IO m, MonadMask m)
  => ConnectionSourceM m
  -> TransactionSettings
  -> DBT m a
  -> m a
runDBT cs ts action = withConnectionData cs ts $ \cd -> do
  evalStateT (unDBT action) $ mkDBState cd ts

-- | Transform the underlying monad.
mapDBT
  :: (DBState n -> DBState m)
  -> (m (a, DBState m) -> n (b, DBState n))
  -> DBT m a
  -> DBT n b
mapDBT f g m = DBT . StateT $ g . runStateT (unDBT m) . f

----------------------------------------

instance (m ~ n, MonadBase IO m, MonadMask m) => MonadDB (DBT_ m n) where
  runQuery sql = withFrozenCallStack $ do
    DBT . StateT $ \st -> withConnection (dbConnectionData st) $ \conn -> do
      liftBase $ updateStateWith conn st sql =<< runQueryIO conn sql
  runPreparedQuery name sql = withFrozenCallStack $ do
    DBT . StateT $ \st -> withConnection (dbConnectionData st) $ \conn -> do
      liftBase $ updateStateWith conn st sql =<< runPreparedQueryIO conn name sql

  getLastQuery = DBT . gets $ dbLastQuery

  withFrozenLastQuery callback = DBT . StateT $ \st -> do
    let st' = st {dbRecordLastQuery = False}
    (x, st'') <- runStateT (unDBT callback) st'
    pure (x, st'' {dbRecordLastQuery = dbRecordLastQuery st})

  getConnectionStats = DBT $ gets dbConnectionStats

  getQueryResult = DBT $ gets dbQueryResult
  clearQueryResult = DBT . modify' $ \st -> st {dbQueryResult = Nothing}

  getConnectionAcquisitionMode = DBT . StateT $ \st -> do
    (,st) <$> liftBase (getConnectionAcquisitionModeIO $ dbConnectionData st)

  acquireAndHoldConnection isolationLevel permissions = DBT . StateT $ \st -> do
    (,st) <$> changeAcquisitionModeTo (AcquireAndHold isolationLevel permissions) (dbConnectionData st)

  unsafeAcquireOnDemandConnection = DBT . StateT $ \st -> do
    (,st) <$> changeAcquisitionModeTo AcquireOnDemand (dbConnectionData st)

  getNotification time = DBT . StateT $ \st -> do
    withConnection (dbConnectionData st) $ \conn -> do
      (,st) <$> liftBase (getNotificationIO conn time)

  withNewConnection m = DBT . StateT $ \st -> do
    cam <- liftBase . getConnectionAcquisitionModeIO $ dbConnectionData st
    let cs = getConnectionSource $ dbConnectionData st
        ts =
          TransactionSettings
            { tsRestartPredicate = dbRestartPredicate st
            , tsConnectionAcquisitionMode = cam
            }
    res <- runDBT cs ts m
    pure (res, st)

----------------------------------------

instance MonadTransControl (DBT_ m) where
  type StT (DBT_ m) a = StT (InnerDBT m) a
  liftWith = defaultLiftWith DBT unDBT
  restoreT = defaultRestoreT DBT

instance (m ~ n, MonadBaseControl b m) => MonadBaseControl b (DBT_ m n) where
  type StM (DBT_ m n) a = ComposeSt (DBT_ m) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance (m ~ n, MonadError e m) => MonadError e (DBT_ m n) where
  throwError = lift . throwError
  catchError m h = DBT $ S.liftCatch catchError (unDBT m) (unDBT . h)

instance (m ~ n, MonadReader r m) => MonadReader r (DBT_ m n) where
  ask = lift ask
  local f = mapDBT id (local f)
  reader = lift . reader

instance (m ~ n, MonadState s m) => MonadState s (DBT_ m n) where
  get = lift get
  put = lift . put
  state = lift . state

instance (m ~ n, MonadWriter w m) => MonadWriter w (DBT_ m n) where
  writer = lift . writer
  tell = lift . tell
  listen = DBT . S.liftListen listen . unDBT
  pass = DBT . S.liftPass pass . unDBT
