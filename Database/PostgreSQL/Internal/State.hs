{-# OPTIONS_GHC -Wall #-}
module Database.PostgreSQL.Internal.State where

import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Concurrent.MVar

import Database.PostgreSQL.Internal.C.Types
import Database.PostgreSQL.Internal.SQL

data TransactionMode = TransactionMode {
  tmAutoTransaction :: !Bool
, tmIsolationLevel  :: !IsolationLevel
, tmPermissions     :: !Permissions
}

data IsolationLevel = DefaultLevel | ReadCommitted | RepeatableRead | Serializable
data Permissions = DefaultPermissions | ReadOnly | ReadWrite

newtype QueryResult = QueryResult { unQueryResult :: ForeignPtr PGresult }

----------------------------------------

data DBState = DBState {
  dbConnection      :: MVar (Maybe (Ptr PGconn))
, dbTransactionMode :: TransactionMode
, dbLastQuery       :: SQL
, dbQueryResult     :: Maybe QueryResult
}
