-- | Definition of internal DBT state.
module Database.PostgreSQL.PQTypes.Internal.State
  ( DBState(..)
  , updateStateWith
  ) where

import Foreign.ForeignPtr

import Database.PostgreSQL.PQTypes.FromRow
import Database.PostgreSQL.PQTypes.Internal.C.Types
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

updateStateWith :: IsSQL sql => DBState m -> sql -> ForeignPtr PGresult -> DBState m
updateStateWith st sql res = st
  { dbLastQuery = if dbRecordLastQuery st then SomeSQL sql else dbLastQuery st
  , dbQueryResult = Just QueryResult
    { qrSQL = SomeSQL sql
    , qrResult = res
    , qrFromRow = id
    }
  }
