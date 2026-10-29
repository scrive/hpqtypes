-- | Hooks for observing queries, e.g. for tracing. Set
-- 'Database.PostgreSQL.PQTypes.csQueryObserver' of the connection settings to
-- observe all queries of a connection.
module Database.PostgreSQL.PQTypes.Observer
  ( -- * Observer
    QueryObserver (..)
  , QueryObservation (..)
  , QueryInfo (..)
  , QueryOutcome (..)
  ) where

import Database.PostgreSQL.PQTypes.Internal.Observer
