{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
module Database.PostgreSQL.PQTypes.Class.Instances where

import Control.Monad.Trans
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Control
import Control.Monad.Trans.Error
import Control.Monad.Trans.Identity
import Control.Monad.Trans.List
import Control.Monad.Trans.Reader
import Data.Monoid
import qualified Control.Monad.Trans.RWS.Lazy as L
import qualified Control.Monad.Trans.RWS.Strict as S
import qualified Control.Monad.Trans.State.Lazy as L
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad.Trans.Writer.Lazy as L
import qualified Control.Monad.Trans.Writer.Strict as S

import Database.PostgreSQL.PQTypes.Class
import Database.PostgreSQL.PQTypes.Fold

-- We all love boilerplate, don't we?

instance (MonadBaseControl IO m, MonadDB m) => MonadDB (ContT e m) where
  runQuery = lift . runQuery
  getLastQuery = lift getLastQuery
  getConnectionStats = lift getConnectionStats
  getQueryResult = lift getQueryResult
  clearQueryResult = lift clearQueryResult
  getTransactionSettings = lift getTransactionSettings
  setTransactionSettings = lift . setTransactionSettings
  foldlM = foldLeftM
  foldrM = foldRightM
  throwDB = lift . throwDB
  withNewConnection = mapContT withNewConnection

instance (Error e, MonadBaseControl IO m, MonadDB m) => MonadDB (ErrorT e m) where
  runQuery = lift . runQuery
  getLastQuery = lift getLastQuery
  getConnectionStats = lift getConnectionStats
  getQueryResult = lift getQueryResult
  clearQueryResult = lift clearQueryResult
  getTransactionSettings = lift getTransactionSettings
  setTransactionSettings = lift . setTransactionSettings
  foldlM = foldLeftM
  foldrM = foldRightM
  throwDB = lift . throwDB
  withNewConnection = mapErrorT withNewConnection

instance (MonadBaseControl IO m, MonadDB m) => MonadDB (IdentityT m) where
  runQuery = lift . runQuery
  getLastQuery = lift getLastQuery
  getConnectionStats = lift getConnectionStats
  getQueryResult = lift getQueryResult
  clearQueryResult = lift clearQueryResult
  getTransactionSettings = lift getTransactionSettings
  setTransactionSettings = lift . setTransactionSettings
  foldlM = foldLeftM
  foldrM = foldRightM
  throwDB = lift . throwDB
  withNewConnection = mapIdentityT withNewConnection

instance (MonadBaseControl IO m, MonadDB m) => MonadDB (ListT m) where
  runQuery = lift . runQuery
  getLastQuery = lift getLastQuery
  getConnectionStats = lift getConnectionStats
  getQueryResult = lift getQueryResult
  clearQueryResult = lift clearQueryResult
  getTransactionSettings = lift getTransactionSettings
  setTransactionSettings = lift . setTransactionSettings
  foldlM = foldLeftM
  foldrM = foldRightM
  throwDB = lift . throwDB
  withNewConnection = mapListT withNewConnection

instance (Monoid w, MonadBaseControl IO m, MonadDB m) => MonadDB (L.RWST r w s m) where
  runQuery = lift . runQuery
  getLastQuery = lift getLastQuery
  getConnectionStats = lift getConnectionStats
  getQueryResult = lift getQueryResult
  clearQueryResult = lift clearQueryResult
  getTransactionSettings = lift getTransactionSettings
  setTransactionSettings = lift . setTransactionSettings
  foldlM = foldLeftM
  foldrM = foldRightM
  throwDB = lift . throwDB
  withNewConnection = L.mapRWST withNewConnection

instance (Monoid w, MonadBaseControl IO m, MonadDB m) => MonadDB (S.RWST r w s m) where
  runQuery = lift . runQuery
  getLastQuery = lift getLastQuery
  getConnectionStats = lift getConnectionStats
  getQueryResult = lift getQueryResult
  clearQueryResult = lift clearQueryResult
  getTransactionSettings = lift getTransactionSettings
  setTransactionSettings = lift . setTransactionSettings
  foldlM = foldLeftM
  foldrM = foldRightM
  throwDB = lift . throwDB
  withNewConnection = S.mapRWST withNewConnection

instance (MonadBaseControl IO m, MonadDB m) => MonadDB (ReaderT r m) where
  runQuery = lift . runQuery
  getLastQuery = lift getLastQuery
  getConnectionStats = lift getConnectionStats
  getQueryResult = lift getQueryResult
  clearQueryResult = lift clearQueryResult
  getTransactionSettings = lift getTransactionSettings
  setTransactionSettings = lift . setTransactionSettings
  foldlM = foldLeftM
  foldrM = foldRightM
  throwDB = lift . throwDB
  withNewConnection = mapReaderT withNewConnection

instance (MonadBaseControl IO m, MonadDB m) => MonadDB (L.StateT s m) where
  runQuery = lift . runQuery
  getLastQuery = lift getLastQuery
  getConnectionStats = lift getConnectionStats
  getQueryResult = lift getQueryResult
  clearQueryResult = lift clearQueryResult
  getTransactionSettings = lift getTransactionSettings
  setTransactionSettings = lift . setTransactionSettings
  foldlM = foldLeftM
  foldrM = foldRightM
  throwDB = lift . throwDB
  withNewConnection = L.mapStateT withNewConnection

instance (MonadBaseControl IO m, MonadDB m) => MonadDB (S.StateT s m) where
  runQuery = lift . runQuery
  getLastQuery = lift getLastQuery
  getConnectionStats = lift getConnectionStats
  getQueryResult = lift getQueryResult
  clearQueryResult = lift clearQueryResult
  getTransactionSettings = lift getTransactionSettings
  setTransactionSettings = lift . setTransactionSettings
  foldlM = foldLeftM
  foldrM = foldRightM
  throwDB = lift . throwDB
  withNewConnection = S.mapStateT withNewConnection

instance (Monoid w, MonadBaseControl IO m, MonadDB m) => MonadDB (L.WriterT w m) where
  runQuery = lift . runQuery
  getLastQuery = lift getLastQuery
  getConnectionStats = lift getConnectionStats
  getQueryResult = lift getQueryResult
  clearQueryResult = lift clearQueryResult
  getTransactionSettings = lift getTransactionSettings
  setTransactionSettings = lift . setTransactionSettings
  foldlM = foldLeftM
  foldrM = foldRightM
  throwDB = lift . throwDB
  withNewConnection = L.mapWriterT withNewConnection

instance (Monoid w, MonadBaseControl IO m, MonadDB m) => MonadDB (S.WriterT w m) where
  runQuery = lift . runQuery
  getLastQuery = lift getLastQuery
  getConnectionStats = lift getConnectionStats
  getQueryResult = lift getQueryResult
  clearQueryResult = lift clearQueryResult
  getTransactionSettings = lift getTransactionSettings
  setTransactionSettings = lift . setTransactionSettings
  foldlM = foldLeftM
  foldrM = foldRightM
  throwDB = lift . throwDB
  withNewConnection = S.mapWriterT withNewConnection
