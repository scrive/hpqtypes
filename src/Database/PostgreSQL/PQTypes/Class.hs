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
  -- | Run SQL query and return number of affected/returned rows. Note that
  -- for a given connection, only one thread may be executing 'runQuery' at
  -- a given time. If simultaneous call is made from another thread, it
  -- will block until currently running 'runQuery' finishes.
  runQuery     :: IsSQL sql => sql -> m Int
  -- | Get last SQL query that was executed.
  getLastQuery :: m SomeSQL

  -- | Get current connection statistics.
  getConnectionStats :: m ConnectionStats

  -- | Get current query result.
  getQueryResult   :: m (Maybe QueryResult)
  -- | Clear current query result.
  clearQueryResult :: m ()

  -- | Get current transaction settings.
  getTransactionSettings :: m TransactionSettings
  -- | Set transaction settings to supplied ones. Note that it
  -- won't change any properties of currently running transaction,
  -- only the subsequent ones.
  setTransactionSettings :: TransactionSettings -> m ()

  -- | Fold the result set of rows from left to right.
  foldlM :: FromRow row => (acc -> row -> m acc) -> acc -> m acc
  -- | Fold the result set of rows from right to left.
  foldrM :: FromRow row => (row -> acc -> m acc) -> acc -> m acc

  -- | Throw supplied exception as 'DBException'.
  throwDB :: Exception e => e -> m a

  -- | Execute supplied monadic action with new connection
  -- using current connection source and transaction settings.
  --
  -- Particularly useful when you want to spawn a new thread, but
  -- do not want the connection in child thread to be shared with
  -- the parent one.
  withNewConnection :: m a -> m a
