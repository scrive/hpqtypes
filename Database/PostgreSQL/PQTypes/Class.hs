{-# OPTIONS_GHC -Wall #-}
module Database.PostgreSQL.PQTypes.Class where

import Database.PostgreSQL.PQTypes.Internal.Connection
import Database.PostgreSQL.PQTypes.Internal.State
import Database.PostgreSQL.PQTypes.Internal.SQL
import Database.PostgreSQL.PQTypes.Row

class Monad m => MonadDB m where
  runQuery :: SQL -> m Int

  getLastQuery   :: m SQL
  clearLastQuery :: m ()

  getQueryResult   :: m (Maybe QueryResult)
  clearQueryResult :: m ()

  getTransactionSettings :: m TransactionSettings
  setTransactionSettings :: TransactionSettings -> m ()

  foldlDB :: Row row => (acc -> row -> m acc) -> acc -> m acc
  foldrDB :: Row row => (row -> acc -> m acc) -> acc -> m acc

  getConnectionSource :: m ConnectionSource
  localConnection     :: (Connection -> Connection) -> m a -> m a
