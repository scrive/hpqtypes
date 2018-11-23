-- | Definition of main exception type.
module Database.PostgreSQL.PQTypes.Internal.Exception (
    DBException(..)
  , rethrowWithContext
  ) where

import qualified Control.Exception as E

import Database.PostgreSQL.PQTypes.SQL.Class

-- | Main exception type. All exceptions thrown by
-- the library are additionally wrapped in this type.
data DBException = forall e sql. (E.Exception e, Show sql) => DBException {
-- | Last SQL query that was executed.
  dbeQueryContext :: !sql
-- | Specific error.
, dbeError        :: !e
}

deriving instance Show DBException

instance E.Exception DBException

-- | Rethrow supplied exception enriched with given SQL.
rethrowWithContext :: IsSQL sql => sql -> E.SomeException -> IO a
rethrowWithContext sql (E.SomeException e) = E.throwIO DBException {
    dbeQueryContext = sql
  , dbeError = e
  }
