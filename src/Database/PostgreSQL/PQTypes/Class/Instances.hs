{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
module Database.PostgreSQL.PQTypes.Class.Instances where

import Control.Monad.Trans
import Control.Monad.Trans.Error
import Control.Monad.Trans.Identity
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Data.Monoid
import qualified Control.Monad.Trans.RWS.Lazy as L
import qualified Control.Monad.Trans.RWS.Strict as S
import qualified Control.Monad.Trans.State.Lazy as L
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad.Trans.Writer.Lazy as L
import qualified Control.Monad.Trans.Writer.Strict as S

import Database.PostgreSQL.PQTypes.Class

instance (Error e, MonadDB m) => MonadDB (ErrorT e m) where
  runQuery = lift . runQuery
  getLastQuery = lift getLastQuery
  getConnectionStats = lift getConnectionStats
  getQueryResult = lift getQueryResult
  clearQueryResult = lift clearQueryResult
  getTransactionSettings = lift getTransactionSettings
  setTransactionSettings = lift . setTransactionSettings
  getNotification = lift . getNotification
  withNewConnection = mapErrorT withNewConnection

instance MonadDB m => MonadDB (IdentityT m) where
  runQuery = lift . runQuery
  getLastQuery = lift getLastQuery
  getConnectionStats = lift getConnectionStats
  getQueryResult = lift getQueryResult
  clearQueryResult = lift clearQueryResult
  getTransactionSettings = lift getTransactionSettings
  setTransactionSettings = lift . setTransactionSettings
  getNotification = lift . getNotification
  withNewConnection = mapIdentityT withNewConnection

instance MonadDB m => MonadDB (ListT m) where
  runQuery = lift . runQuery
  getLastQuery = lift getLastQuery
  getConnectionStats = lift getConnectionStats
  getQueryResult = lift getQueryResult
  clearQueryResult = lift clearQueryResult
  getTransactionSettings = lift getTransactionSettings
  setTransactionSettings = lift . setTransactionSettings
  getNotification = lift . getNotification
  withNewConnection = mapListT withNewConnection

instance MonadDB m => MonadDB (MaybeT m) where
  runQuery = lift . runQuery
  getLastQuery = lift getLastQuery
  getConnectionStats = lift getConnectionStats
  getQueryResult = lift getQueryResult
  clearQueryResult = lift clearQueryResult
  getTransactionSettings = lift getTransactionSettings
  setTransactionSettings = lift . setTransactionSettings
  getNotification = lift . getNotification
  withNewConnection = mapMaybeT withNewConnection

instance (Monoid w, MonadDB m) => MonadDB (L.RWST r w s m) where
  runQuery = lift . runQuery
  getLastQuery = lift getLastQuery
  getConnectionStats = lift getConnectionStats
  getQueryResult = lift getQueryResult
  clearQueryResult = lift clearQueryResult
  getTransactionSettings = lift getTransactionSettings
  setTransactionSettings = lift . setTransactionSettings
  getNotification = lift . getNotification
  withNewConnection = L.mapRWST withNewConnection

instance (Monoid w, MonadDB m) => MonadDB (S.RWST r w s m) where
  runQuery = lift . runQuery
  getLastQuery = lift getLastQuery
  getConnectionStats = lift getConnectionStats
  getQueryResult = lift getQueryResult
  clearQueryResult = lift clearQueryResult
  getTransactionSettings = lift getTransactionSettings
  setTransactionSettings = lift . setTransactionSettings
  getNotification = lift . getNotification
  withNewConnection = S.mapRWST withNewConnection

instance MonadDB m => MonadDB (ReaderT r m) where
  runQuery = lift . runQuery
  getLastQuery = lift getLastQuery
  getConnectionStats = lift getConnectionStats
  getQueryResult = lift getQueryResult
  clearQueryResult = lift clearQueryResult
  getTransactionSettings = lift getTransactionSettings
  setTransactionSettings = lift . setTransactionSettings
  getNotification = lift . getNotification
  withNewConnection = mapReaderT withNewConnection

instance MonadDB m => MonadDB (L.StateT s m) where
  runQuery = lift . runQuery
  getLastQuery = lift getLastQuery
  getConnectionStats = lift getConnectionStats
  getQueryResult = lift getQueryResult
  clearQueryResult = lift clearQueryResult
  getTransactionSettings = lift getTransactionSettings
  setTransactionSettings = lift . setTransactionSettings
  getNotification = lift . getNotification
  withNewConnection = L.mapStateT withNewConnection

instance MonadDB m => MonadDB (S.StateT s m) where
  runQuery = lift . runQuery
  getLastQuery = lift getLastQuery
  getConnectionStats = lift getConnectionStats
  getQueryResult = lift getQueryResult
  clearQueryResult = lift clearQueryResult
  getTransactionSettings = lift getTransactionSettings
  setTransactionSettings = lift . setTransactionSettings
  getNotification = lift . getNotification
  withNewConnection = S.mapStateT withNewConnection

instance (Monoid w, MonadDB m) => MonadDB (L.WriterT w m) where
  runQuery = lift . runQuery
  getLastQuery = lift getLastQuery
  getConnectionStats = lift getConnectionStats
  getQueryResult = lift getQueryResult
  clearQueryResult = lift clearQueryResult
  getTransactionSettings = lift getTransactionSettings
  setTransactionSettings = lift . setTransactionSettings
  getNotification = lift . getNotification
  withNewConnection = L.mapWriterT withNewConnection

instance (Monoid w, MonadDB m) => MonadDB (S.WriterT w m) where
  runQuery = lift . runQuery
  getLastQuery = lift getLastQuery
  getConnectionStats = lift getConnectionStats
  getQueryResult = lift getQueryResult
  clearQueryResult = lift clearQueryResult
  getTransactionSettings = lift getTransactionSettings
  setTransactionSettings = lift . setTransactionSettings
  getNotification = lift . getNotification
  withNewConnection = S.mapWriterT withNewConnection
