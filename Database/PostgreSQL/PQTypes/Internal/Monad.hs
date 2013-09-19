{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
module Database.PostgreSQL.PQTypes.Internal.Monad where

import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Trans
import Control.Monad.Trans.RWS
import Data.Monoid

import Database.PostgreSQL.PQTypes.Class
import Database.PostgreSQL.PQTypes.Fold
import Database.PostgreSQL.PQTypes.Internal.Connection
import Database.PostgreSQL.PQTypes.Internal.State
import Database.PostgreSQL.PQTypes.Internal.Transaction
import qualified Database.PostgreSQL.PQTypes.Internal.Query as Q

type InnerDBT = RWST Connection () DBState

newtype DBT m a = DBT { unDBT :: InnerDBT m a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadIO, MonadTrans)

runDBT :: MonadBaseControl IO m
       => ConnectionSource -> TransactionSettings -> DBT m a -> m a
runDBT cs ts m = withConnection cs $ \conn -> do
  fst `liftM` evalRWST action conn (DBState {
    dbConnectionSource = cs
  , dbTransactionSettings = ts
  , dbLastQuery = mempty
  , dbQueryResult = Nothing
  })
  where
    action = unDBT $ if tsAutoTransaction ts
      then withTransaction' (ts { tsAutoTransaction = False }) m
      else m

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

instance MonadBase IO m => MonadDB (DBT m) where
  runQuery = Q.runQuery DBT

  getLastQuery = DBT . gets $ dbLastQuery
  clearLastQuery = DBT . modify $ \st -> st { dbLastQuery = mempty }

  getTransactionSettings = DBT . gets $ dbTransactionSettings
  setTransactionSettings ts = DBT . modify $ \st -> st { dbTransactionSettings = ts }

  getQueryResult = DBT $ gets dbQueryResult
  clearQueryResult = DBT . modify $ \st -> st { dbQueryResult = Nothing }

  foldlDB = foldLeft
  foldrDB = foldRight

  getConnectionSource = DBT . gets $ dbConnectionSource
  localConnection f = DBT . local f . unDBT
