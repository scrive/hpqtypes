{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
module Database.PostgreSQL.PQTypes.Internal.Monad (
    DBT(..)
  , runDBT
  , mapDBT
  ) where

import Control.Applicative
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.Monoid

import Database.PostgreSQL.PQTypes.Class
import Database.PostgreSQL.PQTypes.Fold
import Database.PostgreSQL.PQTypes.Internal.Connection
import Database.PostgreSQL.PQTypes.Internal.Query
import Database.PostgreSQL.PQTypes.Internal.State
import Database.PostgreSQL.PQTypes.Internal.Transaction

type InnerDBT = StateT DBState

newtype DBT m a = DBT { unDBT :: InnerDBT m a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadIO, MonadTrans)

runDBT :: MonadBaseControl IO m
       => ConnectionSource -> TransactionSettings -> DBT m a -> m a
runDBT cs ts m = withConnection cs $ \conn -> do
  evalStateT action $ DBState {
    dbConnection = conn
  , dbConnectionSource = cs
  , dbTransactionSettings = ts
  , dbLastQuery = mempty
  , dbQueryResult = Nothing
  }
  where
    action = unDBT $ if tsAutoTransaction ts
      then withTransaction' (ts { tsAutoTransaction = False }) m
      else m

mapDBT :: (Monad m, Monad n) => (m a -> n b) -> DBT m a -> DBT n b
mapDBT f m = DBT . StateT $ \st -> do
  res <- f $ evalStateT (unDBT m) st
  return (res, st)

instance MonadTransControl DBT where
  newtype StT DBT a = StDBT { unStDBT :: StT InnerDBT a }
  liftWith = defaultLiftWith DBT unDBT StDBT
  restoreT = defaultRestoreT DBT unStDBT
  {-# INLINE liftWith #-}
  {-# INLINE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (DBT m) where
  newtype StM (DBT m) a = StMDBT { unStMDBT :: ComposeSt DBT m a }
  liftBaseWith = defaultLiftBaseWith StMDBT
  restoreM     = defaultRestoreM unStMDBT
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

instance MonadBaseControl IO m => MonadDB (DBT m) where
  runQuery = runSQLQuery DBT
  getLastQuery = DBT . gets $ dbLastQuery

  getTransactionSettings = DBT . gets $ dbTransactionSettings
  setTransactionSettings ts = DBT . modify $ \st -> st { dbTransactionSettings = ts }

  getQueryResult = DBT $ gets dbQueryResult
  clearQueryResult = DBT . modify $ \st -> st { dbQueryResult = Nothing }

  foldlM = foldLeftM
  foldrM = foldRightM

  withNewConnection m = DBT . StateT $ \st -> do
    let cs = dbConnectionSource st
        ts = dbTransactionSettings st
    res <- runDBT cs ts m
    return (res, st)
