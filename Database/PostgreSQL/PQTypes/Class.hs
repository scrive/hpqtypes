{-# OPTIONS_GHC -Wall #-}
module Database.PostgreSQL.PQTypes.Class (
    MonadDB(..)
  ) where

import Database.PostgreSQL.PQTypes.FromRow
import Database.PostgreSQL.PQTypes.Internal.State
import Database.PostgreSQL.PQTypes.SQL.Class

class Monad m => MonadDB m where
  runQuery     :: IsSQL sql => sql -> m Int
  getLastQuery :: m SomeSQL

  getQueryResult   :: m (Maybe QueryResult)
  clearQueryResult :: m ()

  getTransactionSettings :: m TransactionSettings
  setTransactionSettings :: TransactionSettings -> m ()

  foldlM :: FromRow row => (acc -> row -> m acc) -> acc -> m acc
  foldrM :: FromRow row => (row -> acc -> m acc) -> acc -> m acc

  withNewConnection :: m a -> m a
