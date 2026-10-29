-- | Hooks for observing queries, e.g. for tracing.
module Database.PostgreSQL.PQTypes.Internal.Observer
  ( QueryObserver (..)
  , QueryObservation (..)
  , QueryInfo (..)
  , QueryOutcome (..)
  ) where

import Data.Text qualified as T
import GHC.Stack

import Database.PostgreSQL.PQTypes.Internal.BackendPid

-- | Observer of the queries that a connection runs. This includes the queries
-- that the library runs itself, e.g. @BEGIN@ and @COMMIT@ of transactions.
--
-- A query runs in a separate thread, so that it can be interrupted, and the
-- observation is split in two steps. 'startQueryObservation' runs in the thread
-- that requested the query, so it can e.g. capture the thread-local context of
-- a tracing library. 'observeQuery' of the result then wraps the query itself,
-- possibly in another thread.
newtype QueryObserver = QueryObserver
  { startQueryObservation :: IO QueryObservation
  }

-- | All observers are equal, so that 'Database.PostgreSQL.PQTypes.ConnectionSettings'
-- can keep its 'Eq' instance.
instance Eq QueryObserver where
  _ == _ = True

-- | All observers are equal, so that 'Database.PostgreSQL.PQTypes.ConnectionSettings'
-- can keep its 'Ord' instance.
instance Ord QueryObserver where
  compare _ _ = EQ

instance Show QueryObserver where
  showsPrec _ _ = showString "<QueryObserver>"

-- | Observation of a single query, see 'QueryObserver'.
newtype QueryObservation = QueryObservation
  { observeQuery :: forall r. QueryInfo -> IO (QueryOutcome, r) -> IO r
  -- ^ Wrap execution of a query. The action throws an exception if the query
  -- fails, e.g. 'Database.PostgreSQL.PQTypes.DBException' with
  -- 'Database.PostgreSQL.PQTypes.DetailedQueryError'.
  }

-- | Information about a query, available before it runs.
data QueryInfo = QueryInfo
  { qiQuery :: !T.Text
  -- ^ Text of the query as sent to the server. Parameters are sent separately,
  -- so the text contains placeholders such as @$1@ in place of their values.
  , qiPreparedName :: !(Maybe T.Text)
  -- ^ Name of the prepared statement, if the query runs as one.
  , qiBackendPid :: !BackendPid
  -- ^ Process ID of the server process that runs the query.
  , qiDatabase :: !(Maybe T.Text)
  -- ^ Name of the database of the connection.
  , qiHost :: !(Maybe T.Text)
  -- ^ Host of the server, or the directory of its Unix-domain socket.
  , qiPort :: !(Maybe T.Text)
  -- ^ Port of the server.
  , qiCallStack :: CallStack
  -- ^ Call stack of the code that runs the query.
  }

-- | Result of a successful query.
data QueryOutcome
  = -- | Number of rows a command such as @UPDATE@ affected.
    AffectedRows !Int
  | -- | Number of rows a query returned.
    ReturnedRows !Int
  deriving (Eq, Ord, Show)
