{-# LANGUAGE CPP #-}
module Database.PostgreSQL.PQTypes.Internal.Monad (
    DBT_(..)
  , DBT
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
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad.Fail as MF

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

type InnerDBT m = StateT (DBState m)

-- | Monad transformer for adding database
-- interaction capabilities to the underlying monad.
newtype DBT_ m n a = DBT { unDBT :: InnerDBT m n a }
  deriving (Alternative, Applicative, Functor, Monad, MF.MonadFail, MonadBase b, MonadCatch, MonadIO, MonadMask, MonadPlus, MonadThrow, MonadTrans)

type DBT m = DBT_ m m

-- | Evaluate monadic action with supplied
-- connection source and transaction settings.
{-# INLINABLE runDBT #-}
runDBT
  :: (MonadBase IO m, MonadMask m)
  => ConnectionSourceM m
  -> TransactionSettings
  -> DBT m a
  -> m a
runDBT cs ts m = withConnection cs $ \conn -> do
  evalStateT action $ DBState {
    dbConnection = conn
  , dbConnectionSource = cs
  , dbTransactionSettings = ts
  , dbLastQuery = SomeSQL (mempty::SQL)
  , dbRecordLastQuery = True
  , dbQueryResult = Nothing
  }
  where
    action = unDBT $ if tsAutoTransaction ts
      then withTransaction' (ts { tsAutoTransaction = False }) m
      else m

-- | Transform the underlying monad.
{-# INLINABLE mapDBT #-}
mapDBT
  :: (DBState n -> DBState m)
  -> (m (a, DBState m) -> n (b, DBState n))
  -> DBT m a
  -> DBT n b
mapDBT f g m = DBT . StateT $ g . runStateT (unDBT m) . f

----------------------------------------

instance (m ~ n, MonadBase IO m, MonadMask m) => MonadDB (DBT_ m n) where
  runQuery sql = DBT . StateT $ liftBase . runQueryIO sql
  getLastQuery = DBT . gets $ dbLastQuery

  withFrozenLastQuery callback = DBT . StateT $ \st -> do
    let st' = st { dbRecordLastQuery = False }
    (x, st'') <- runStateT (unDBT callback) st'
    pure (x, st'' { dbRecordLastQuery = dbRecordLastQuery st })

  getConnectionStats = do
    mconn <- DBT $ liftBase . readMVar =<< gets (unConnection . dbConnection)
    case mconn of
      Nothing -> throwDB $ HPQTypesError "getConnectionStats: no connection"
      Just cd -> return $ cdStats cd

  getQueryResult = DBT . gets $ \st -> dbQueryResult st
  clearQueryResult = DBT . modify $ \st -> st { dbQueryResult = Nothing }

  getTransactionSettings = DBT . gets $ dbTransactionSettings
  setTransactionSettings ts = DBT . modify $ \st -> st { dbTransactionSettings = ts }

  getNotification time = DBT . StateT $ \st -> (, st)
    <$> liftBase (getNotificationIO st time)

  withNewConnection m = DBT . StateT $ \st -> do
    let cs = dbConnectionSource st
        ts = dbTransactionSettings st
    res <- runDBT cs ts m
    return (res, st)

  {-# INLINABLE runQuery #-}
  {-# INLINABLE getLastQuery #-}
  {-# INLINABLE withFrozenLastQuery #-}
  {-# INLINABLE getConnectionStats #-}
  {-# INLINABLE getQueryResult #-}
  {-# INLINABLE clearQueryResult #-}
  {-# INLINABLE getTransactionSettings #-}
  {-# INLINABLE setTransactionSettings #-}
  {-# INLINABLE getNotification #-}
  {-# INLINABLE withNewConnection #-}

----------------------------------------

instance MonadTransControl (DBT_ m) where
  type StT (DBT_ m) a = StT (InnerDBT m) a
  liftWith = defaultLiftWith DBT unDBT
  restoreT = defaultRestoreT DBT
  {-# INLINE liftWith #-}
  {-# INLINE restoreT #-}

instance (m ~ n, MonadBaseControl b m) => MonadBaseControl b (DBT_ m n) where
  type StM (DBT_ m n) a = ComposeSt (DBT_ m) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

instance (m ~ n, MonadError e m) => MonadError e (DBT_ m n) where
  throwError = lift . throwError
  catchError m h = DBT $ S.liftCatch catchError (unDBT m) (unDBT . h)
  {-# INLINE throwError #-}
  {-# INLINE catchError #-}

instance (m ~ n, MonadReader r m) => MonadReader r (DBT_ m n) where
  ask = lift ask
  local f = mapDBT id (local f)
  reader = lift . reader
  {-# INLINE ask #-}
  {-# INLINE local #-}
  {-# INLINE reader #-}

instance (m ~ n, MonadState s m) => MonadState s (DBT_ m n) where
  get = lift get
  put = lift . put
  state = lift . state
  {-# INLINE get #-}
  {-# INLINE put #-}
  {-# INLINE state #-}

instance (m ~ n, MonadWriter w m) => MonadWriter w (DBT_ m n) where
  writer = lift . writer
  tell = lift . tell
  listen = DBT . S.liftListen listen . unDBT
  pass = DBT . S.liftPass pass . unDBT
  {-# INLINE writer #-}
  {-# INLINE tell #-}
  {-# INLINE listen #-}
  {-# INLINE pass #-}
