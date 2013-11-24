{-# OPTIONS_GHC -Wall #-}
module Database.PostgreSQL.PQTypes.Internal.QueryResult (
    QueryResult(..)
  , ntuples
  , nfields
  ) where

import Control.Monad
import Foreign.ForeignPtr
import System.IO.Unsafe

import Database.PostgreSQL.PQTypes.Internal.C.Interface
import Database.PostgreSQL.PQTypes.Internal.C.Types

-- Note: c_PQntuples/c_PQnfields are pure on a C level and QueryResult
-- constructor is not exported to the end user (so it's not possible
-- to enforce premature finalization via finalizeForeignPtr), which
-- makes usage of unsafeDupablePerformIO fine here.

-- | Wrapper for hiding representation of query result.
newtype QueryResult = QueryResult (ForeignPtr PGresult)

-- | Extract number of returned tuples (rows) from query result.
ntuples :: QueryResult -> Int
ntuples (QueryResult fres) = unsafeDupablePerformIO $
  fromIntegral `liftM` withForeignPtr fres c_PQntuples

-- | Extract number of returned fields (columns) from query result.
nfields :: QueryResult -> Int
nfields (QueryResult fres) = unsafeDupablePerformIO $
  fromIntegral `liftM` withForeignPtr fres c_PQnfields
