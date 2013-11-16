{-# OPTIONS_GHC -Wall #-}
module Database.PostgreSQL.PQTypes.Class (
    MonadDB(..)
  ) where

import Control.Applicative
import Control.Exception (Exception)

import Database.PostgreSQL.PQTypes.FromRow
import Database.PostgreSQL.PQTypes.Internal.Connection
import Database.PostgreSQL.PQTypes.Internal.QueryResult
import Database.PostgreSQL.PQTypes.Transaction.Settings
import Database.PostgreSQL.PQTypes.SQL.Class

class (Applicative m, Functor m, Monad m) => MonadDB m where
  runQuery     :: IsSQL sql => sql -> m Int
  getLastQuery :: m SomeSQL

  getConnectionStats :: m ConnectionStats

  getQueryResult   :: m (Maybe QueryResult)
  clearQueryResult :: m ()

  getTransactionSettings :: m TransactionSettings
  setTransactionSettings :: TransactionSettings -> m ()

  foldlM :: FromRow row => (acc -> row -> m acc) -> acc -> m acc
  foldrM :: FromRow row => (row -> acc -> m acc) -> acc -> m acc

  throwDB :: Exception e => e -> m a

  withNewConnection :: m a -> m a
