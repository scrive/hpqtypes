{-# OPTIONS_GHC -Wall #-}
module Database.PostgreSQL.PQTypes.Class (
    MonadDB(..)
  ) where

import Database.PostgreSQL.PQTypes.Internal.State
import Database.PostgreSQL.PQTypes.Internal.SQL
import Database.PostgreSQL.PQTypes.Row

class Monad m => MonadDB m where
  runQuery     :: SQL -> m Int
  getLastQuery :: m SQL

  getQueryResult   :: m (Maybe QueryResult)
  clearQueryResult :: m ()

  getTransactionSettings :: m TransactionSettings
  setTransactionSettings :: TransactionSettings -> m ()

  foldlM :: Row row => (acc -> row -> m acc) -> acc -> m acc
  foldrM :: Row row => (row -> acc -> m acc) -> acc -> m acc

  localConnection :: m a -> m a
