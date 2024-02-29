module Database.PostgreSQL.PQTypes.SQL.Class
  ( SomeSQL (..)
  , IsSQL (..)
  , unsafeSQL
  ) where

import Data.String
import Foreign.C.String
import Foreign.Ptr

import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.ToSQL

-- | Container for SQL-like type storage.
data SomeSQL = forall sql. IsSQL sql => SomeSQL sql

-- | Class representing \"SQLness\" of a given type.
class Show sql => IsSQL sql where
  -- | Convert 'sql' to libpqtypes representation and pass
  -- it to supplied continuation (usually for execution).
  withSQL
    :: sql
    -> ParamAllocator
    -- ^ 'PGparam' allocator.
    -> (Ptr PGparam -> CString -> IO r)
    -- ^ Continuation which takes 'sql'
    -- converted to libpqtypes specific representation, ie. 'PGparam' object
    -- containing query parameters and C string containing the query itself.
    -> IO r

----------------------------------------

-- | Convert unsafely from 'String' to 'sql' (Note: reckless usage
-- of this function may introduce security vulnerabilities such
-- as proneness to SQL injection attacks).
unsafeSQL :: (IsSQL sql, IsString sql) => String -> sql
unsafeSQL = fromString
