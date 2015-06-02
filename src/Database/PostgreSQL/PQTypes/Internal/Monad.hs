{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, ScopedTypeVariables, TupleSections
  , TypeFamilies, UndecidableInstances, CPP #-}
module Database.PostgreSQL.PQTypes.Internal.Monad (
    DBT(..)
  , runDBT
  , mapDBT
  ) where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Strict
import Control.Monad.Trans.Control
import Control.Monad.Writer.Class
import Data.Monoid
import qualified Control.Monad.Trans.State.Strict as S

import Database.PostgreSQL.PQTypes.Class
import Database.PostgreSQL.PQTypes.Internal.Connection
import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.Internal.Notification
import Database.PostgreSQL.PQTypes.Internal.Query
import Database.PostgreSQL.PQTypes.Internal.State
import Database.PostgreSQL.PQTypes.SQL
import Database.PostgreSQL.PQTypes.SQL.Class
import Database.PostgreSQL.PQTypes.Transaction
import Database.PostgreSQL.PQTypes.Transaction.Settings
import Database.PostgreSQL.PQTypes.Utils

type InnerDBT = StateT DBState

-- | Monad transformer for adding database
-- interaction capabilities to the underlying monad.
newtype DBT m a = DBT { unDBT :: InnerDBT m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadBase b, MonadCatch, MonadIO, MonadMask, MonadPlus, MonadThrow, MonadTrans)

-- | Evaluate monadic action with supplied
-- connection source and transaction settings.
runDBT :: (MonadBase IO m, MonadMask m)
       => ConnectionSource -> TransactionSettings -> DBT m a -> m a
runDBT cs ts m = withConnection cs $ \conn -> do
  evalStateT action $ DBState {
    dbConnection = conn
  , dbConnectionSource = cs
  , dbTransactionSettings = ts
  , dbLastQuery = SomeSQL (mempty::SQL)
  , dbQueryResult = Nothing
  }
  where
    action = unDBT $ if tsAutoTransaction ts
      then withTransaction' (ts { tsAutoTransaction = False }) m
      else m

-- | Transform the underlying monad.
mapDBT :: (m (a, DBState) -> n (b, DBState)) -> DBT m a -> DBT n b
mapDBT f = DBT . mapStateT f . unDBT

----------------------------------------

instance (MonadBase IO m, MonadMask m) => MonadDB (DBT m) where
  runQuery sql = DBT . StateT $ liftBase . runQueryIO sql
  getLastQuery = DBT . gets $ dbLastQuery

  getConnectionStats = do
    mconn <- DBT $ liftBase . readMVar =<< gets (unConnection . dbConnection)
    case mconn of
      Nothing -> throwDB $ HPQTypesError "getConnectionStats: no connection"
      Just cd -> return $ cdStats cd

  getTransactionSettings = DBT . gets $ dbTransactionSettings
  setTransactionSettings ts = DBT . modify $ \st -> st { dbTransactionSettings = ts }

  getQueryResult = DBT . gets $ dbQueryResult
  clearQueryResult = DBT . modify $ \st -> st { dbQueryResult = Nothing }

  getNotification time = DBT . StateT $ \st -> (, st)
    <$> liftBase (getNotificationIO st time)

  withNewConnection m = DBT . StateT $ \st -> do
    let cs = dbConnectionSource st
        ts = dbTransactionSettings st
    res <- runDBT cs ts m
    return (res, st)

----------------------------------------

instance MonadTransControl DBT where
#if MIN_VERSION_monad_control(1,0,0)
  type StT DBT a = StT InnerDBT a
  liftWith = defaultLiftWith DBT unDBT
  restoreT = defaultRestoreT DBT
  {-# INLINE liftWith #-}
  {-# INLINE restoreT #-}
#else
  newtype StT DBT a = StDBT { unStDBT :: StT InnerDBT a }
  liftWith = defaultLiftWith DBT unDBT StDBT
  restoreT = defaultRestoreT DBT unStDBT
  {-# INLINE liftWith #-}
  {-# INLINE restoreT #-}
#endif

instance MonadBaseControl b m => MonadBaseControl b (DBT m) where
#if MIN_VERSION_monad_control(1,0,0)
  type StM (DBT m) a = ComposeSt DBT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}
#else
  newtype StM (DBT m) a = StMDBT { unStMDBT :: ComposeSt DBT m a }
  liftBaseWith = defaultLiftBaseWith StMDBT
  restoreM     = defaultRestoreM unStMDBT
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}
#endif

instance MonadError e m => MonadError e (DBT m) where
  throwError = lift . throwError
  catchError m h = DBT $ S.liftCatch catchError (unDBT m) (unDBT . h)

instance MonadReader r m => MonadReader r (DBT m) where
  ask = lift ask
  local f = mapDBT (local f)
  reader = lift . reader

instance MonadState s m => MonadState s (DBT m) where
  get = lift get
  put = lift . put
  state = lift . state

instance MonadWriter w m => MonadWriter w (DBT m) where
  writer = lift . writer
  tell = lift . tell
  listen = DBT . S.liftListen listen . unDBT
  pass = DBT . S.liftPass pass . unDBT
