{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
module Database.PostgreSQL.PQTypes.Class.Instances where

import Control.Applicative
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
  foldlM f = ErrorT . foldlM (\acc row ->
    either (return . Left) (\k -> runErrorT $ f k row) acc) . Right
  foldrM f = ErrorT . foldrM (\row acc ->
    either (return . Left) (\k -> runErrorT $ f row k) acc) . Right
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
  foldlM f = IdentityT . foldlM (\acc row -> runIdentityT $ f acc row)
  foldrM f = IdentityT . foldrM (\row acc -> runIdentityT $ f row acc)
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
  foldlM f = ListT . foldlM (\acc row ->
    concat <$> mapM (\k -> runListT $ f k row) acc) . return
  foldrM f = ListT . foldrM (\row acc ->
    concat <$> mapM (\k -> runListT $ f row k) acc) . return
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
  foldlM f = MaybeT . foldlM (\acc row ->
    maybe (return Nothing) (\k -> runMaybeT $ f k row) acc) . Just
  foldrM f = MaybeT . foldrM (\row acc ->
    maybe (return Nothing) (\k -> runMaybeT $ f row k) acc) . Just
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
  foldlM f acc = L.RWST $ \r s -> foldlM (\ ~(acc', s', w) row -> do
    ~(a, s'', w') <- L.runRWST (f acc' row) r s'
    return (a, s'', w `mappend` w')) (acc, s, mempty)
  foldrM f acc = L.RWST $ \r s -> foldrM (\row ~(acc', s', w) -> do
    ~(a, s'', w') <- L.runRWST (f row acc') r s'
    return (a, s'', w `mappend` w')) (acc, s, mempty)
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
  foldlM f acc = S.RWST $ \r s -> foldlM (\(acc', s', w) row -> do
    (a, s'', w') <- S.runRWST (f acc' row) r s'
    return (a, s'', w `mappend` w')) (acc, s, mempty)
  foldrM f acc = S.RWST $ \r s -> foldrM (\row (acc', s', w) -> do
    (a, s'', w') <- S.runRWST (f row acc') r s'
    return (a, s'', w `mappend` w')) (acc, s, mempty)
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
  foldlM f acc = ReaderT $ \r -> foldlM (\acc' row ->
    runReaderT (f acc' row) r) acc
  foldrM f acc = ReaderT $ \r -> foldrM (\row acc' ->
    runReaderT (f row acc') r) acc
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
  foldlM f acc = L.StateT $ \s -> foldlM (\ ~(acc', s') row ->
    L.runStateT (f acc' row) s') (acc, s)
  foldrM f acc = L.StateT $ \s -> foldrM (\row ~(acc', s') ->
    L.runStateT (f row acc') s') (acc, s)
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
  foldlM f acc = S.StateT $ \s -> foldlM (\(acc', s') row ->
    S.runStateT (f acc' row) s') (acc, s)
  foldrM f acc = S.StateT $ \s -> foldrM (\row (acc', s') ->
    S.runStateT (f row acc') s') (acc, s)
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
  foldlM f acc = L.WriterT $ foldlM (\ ~(acc', w) row -> do
    ~(r, w') <- L.runWriterT $ f acc' row
    return (r, w `mappend` w')) (acc, mempty)
  foldrM f acc = L.WriterT $ foldrM (\ row ~(acc', w) -> do
    ~(r, w') <- L.runWriterT $ f row acc'
    return (r, w `mappend` w')) (acc, mempty)
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
  foldlM f acc = S.WriterT $ foldlM (\ (acc', w) row -> do
    (r, w') <- S.runWriterT $ f acc' row
    return (r, w `mappend` w')) (acc, mempty)
  foldrM f acc = S.WriterT $ foldrM (\ row (acc', w) -> do
    (r, w') <- S.runWriterT $ f row acc'
    return (r, w `mappend` w')) (acc, mempty)
  getNotification = lift . getNotification
  withNewConnection = S.mapWriterT withNewConnection
