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
  -- | Convert 'sql' to a C string containing the query (with parameters
  -- represented by placeholders @$1@, @$2@, ...) along with the list of these
  -- parameters and pass them to the supplied continuation (usually for
  -- execution).
  withSQL
    :: sql
    -> (CString -> [PQParam] -> IO r)
    -> IO r

----------------------------------------

-- | Convert unsafely from 'String' to 'sql' (Note: reckless usage
-- of this function may introduce security vulnerabilities such
-- as proneness to SQL injection attacks).
unsafeSQL :: (IsSQL sql, IsString sql) => String -> sql
unsafeSQL = fromString
