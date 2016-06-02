{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Definition of generic 'MonadDB' instance which works for
-- any transformer with 'MonadTrans' and 'MonadTransControl'
-- instances. Quite convenient, but not imported by default
-- due to OverlappingInstances requirement.
module Database.PostgreSQL.PQTypes.Class.Instances.Overlapping () where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Database.PostgreSQL.PQTypes.Class

instance (
    Applicative (t m)
  , Monad (t m)
  , MonadTrans t
  , MonadTransControl t
  , MonadDB m
  ) => MonadDB (t m) where
    runQuery = lift . runQuery
    getLastQuery = lift getLastQuery
    getConnectionStats = lift getConnectionStats
    getQueryResult = lift getQueryResult
    clearQueryResult = lift clearQueryResult
    getTransactionSettings = lift getTransactionSettings
    setTransactionSettings = lift . setTransactionSettings
    getNotification = lift . getNotification
    withNewConnection m = controlT $ \run ->
      withNewConnection (run m)

controlT :: (MonadTransControl t, Monad (t m), Monad m)
         => (Run t -> m (StT t a)) -> t m a
controlT f = liftWith f >>= restoreT . return
