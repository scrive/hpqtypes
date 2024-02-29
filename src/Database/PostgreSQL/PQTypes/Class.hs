module Database.PostgreSQL.PQTypes.Class
  ( QueryName (..)
  , MonadDB (..)
  ) where

import Control.Monad.Trans
import Control.Monad.Trans.Control
import GHC.Stack

import Database.PostgreSQL.PQTypes.FromRow
import Database.PostgreSQL.PQTypes.Internal.Connection
import Database.PostgreSQL.PQTypes.Internal.Notification
import Database.PostgreSQL.PQTypes.Internal.QueryResult
import Database.PostgreSQL.PQTypes.SQL.Class
import Database.PostgreSQL.PQTypes.Transaction.Settings

class (Applicative m, Monad m) => MonadDB m where
  -- | Run SQL query and return number of affected/returned rows. Note that
  -- for a given connection, only one thread may be executing 'runQuery' at
  -- a given time. If simultaneous call is made from another thread, it
  -- will block until currently running 'runQuery' finishes.
  runQuery :: (HasCallStack, IsSQL sql) => sql -> m Int

  -- | Similar to 'runQuery', but it prepares and executes a statement under a
  -- given name.
  runPreparedQuery :: (HasCallStack, IsSQL sql) => QueryName -> sql -> m Int

  -- | Get last SQL query that was executed.
  getLastQuery :: m SomeSQL

  -- | Subsequent queries in the callback do not alter the result of
  -- 'getLastQuery'.
  withFrozenLastQuery :: m a -> m a

  -- | Get ID of the server process attached to the current session.
  getBackendPid :: m Int

  -- | Get current connection statistics.
  getConnectionStats :: HasCallStack => m ConnectionStats

  -- | Get current query result.
  getQueryResult :: FromRow row => m (Maybe (QueryResult row))

  -- | Clear current query result.
  clearQueryResult :: m ()

  -- | Get current transaction settings.
  getTransactionSettings :: m TransactionSettings

  -- | Set transaction settings to supplied ones. Note that it
  -- won't change any properties of currently running transaction,
  -- only the subsequent ones.
  setTransactionSettings :: TransactionSettings -> m ()

  -- | Attempt to receive a notification from the server. This
  -- function waits until a notification arrives or specified
  -- number of microseconds has passed. If a negative number
  -- of microseconds is passed as an argument, it will wait
  -- indefinitely. In addition, there are a couple of things
  -- to be aware of:
  --
  -- * A lock on the underlying database connection is acquired
  -- for the duration of the function.
  --
  -- * Notifications can be received only between transactions
  -- (see <http://www.postgresql.org/docs/current/static/sql-notify.html>
  -- for further info), therefore calling this function within
  -- a transaction block will return 'Just' only if notifications
  -- were received before the transaction began.
  getNotification :: Int -> m (Maybe Notification)

  -- | Execute supplied monadic action with new connection
  -- using current connection source and transaction settings.
  --
  -- Particularly useful when you want to spawn a new thread, but
  -- do not want the connection in child thread to be shared with
  -- the parent one.
  withNewConnection :: m a -> m a

-- | Generic, overlappable instance.
instance
  {-# OVERLAPPABLE #-}
  ( Applicative (t m)
  , Monad (t m)
  , MonadTrans t
  , MonadTransControl t
  , MonadDB m
  )
  => MonadDB (t m)
  where
  runQuery = withFrozenCallStack $ lift . runQuery
  runPreparedQuery name = withFrozenCallStack $ lift . runPreparedQuery name
  getLastQuery = lift getLastQuery
  withFrozenLastQuery m = controlT $ \run -> withFrozenLastQuery (run m)
  getBackendPid = lift getBackendPid
  getConnectionStats = withFrozenCallStack $ lift getConnectionStats
  getQueryResult = lift getQueryResult
  clearQueryResult = lift clearQueryResult
  getTransactionSettings = lift getTransactionSettings
  setTransactionSettings = lift . setTransactionSettings
  getNotification = lift . getNotification
  withNewConnection m = controlT $ \run -> withNewConnection (run m)
