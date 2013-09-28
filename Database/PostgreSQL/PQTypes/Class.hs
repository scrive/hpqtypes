{-# OPTIONS_GHC -Wall #-}
module Database.PostgreSQL.PQTypes.Class (
    MonadDB(..)
  ) where

import Database.PostgreSQL.PQTypes.FromRow
import Database.PostgreSQL.PQTypes.Internal.State
import Database.PostgreSQL.PQTypes.Internal.SQL

class Monad m => MonadDB m where
  runQuery     :: SQL -> m Int
  getLastQuery :: m SQL

  getQueryResult   :: m (Maybe QueryResult)
  clearQueryResult :: m ()

  getTransactionSettings :: m TransactionSettings
  setTransactionSettings :: TransactionSettings -> m ()

  foldlM :: FromRow row => (acc -> row -> m acc) -> acc -> m acc
  foldrM :: FromRow row => (row -> acc -> m acc) -> acc -> m acc

  withNewConnection :: m a -> m a
