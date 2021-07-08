-- | Definition of internal DBT state.
module Database.PostgreSQL.PQTypes.Internal.State (
    DBState(..)
  ) where

import Database.PostgreSQL.PQTypes.FromRow
import Database.PostgreSQL.PQTypes.Internal.Connection
import Database.PostgreSQL.PQTypes.Internal.QueryResult
import Database.PostgreSQL.PQTypes.SQL.Class
import Database.PostgreSQL.PQTypes.Transaction.Settings

-- | Internal DB state.
data DBState m = DBState
  { -- | Active connection.
    dbConnection          :: !Connection
    -- | Supplied connection source.
  , dbConnectionSource    :: !(ConnectionSourceM m)
    -- | Current transaction settings.
  , dbTransactionSettings :: !TransactionSettings
    -- | Last SQL query that was executed.
  , dbLastQuery           :: !SomeSQL
    -- | Whether running query should override 'dbLastQuery'.
  , dbRecordLastQuery     :: !Bool
    -- | Current query result.
  , dbQueryResult         :: !(forall row. FromRow row => Maybe (QueryResult row))
  }
