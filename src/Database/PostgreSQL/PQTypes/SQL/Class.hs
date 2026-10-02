module Database.PostgreSQL.PQTypes.SQL.Class
  ( SomeSQL (..)
  , IsSQL (..)
  , unsafeSQL
  ) where

import Data.String
import Foreign.C.String

import Database.PostgreSQL.PQTypes.ToSQL

-- | Container for SQL-like type storage.
data SomeSQL = forall sql. IsSQL sql => SomeSQL sql

-- | Class representing \"SQLness\" of a given type.
class Show sql => IsSQL sql where
  -- | Pass the query as a C string, along with the list of its parameters,
  -- to the continuation. Placeholders @$1@, @$2@, ... in the query stand for
  -- the parameters.
  withSQL
    :: sql
    -> (CString -> [PQParam] -> IO r)
    -> IO r

----------------------------------------

-- | Convert unsafely from 'String' to @sql@ (Note: reckless usage
-- of this function may introduce security vulnerabilities such
-- as proneness to SQL injection attacks).
unsafeSQL :: (IsSQL sql, IsString sql) => String -> sql
unsafeSQL = fromString
