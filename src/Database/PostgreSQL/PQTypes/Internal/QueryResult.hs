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

newtype QueryResult = QueryResult (ForeignPtr PGresult)

ntuples :: QueryResult -> Int
ntuples (QueryResult fres) = unsafeDupablePerformIO $
  fromIntegral `liftM` withForeignPtr fres c_PQntuples

nfields :: QueryResult -> Int
nfields (QueryResult fres) = unsafeDupablePerformIO $
  fromIntegral `liftM` withForeignPtr fres c_PQnfields
