{-# OPTIONS_GHC -Wall #-}
module Database.PostgreSQL.PQTypes.Internal.State where

import Foreign.ForeignPtr

import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Connection
import Database.PostgreSQL.PQTypes.Internal.SQL

data TransactionSettings = TransactionSettings {
  tsAutoTransaction    :: !Bool
, tsIsolationLevel     :: !IsolationLevel
, tsPermissions        :: !Permissions
}

data IsolationLevel = DefaultLevel | ReadCommitted | RepeatableRead | Serializable
data Permissions = DefaultPermissions | ReadOnly | ReadWrite

newtype QueryResult = QueryResult { unQueryResult :: ForeignPtr PGresult }

----------------------------------------

data DBState = DBState {
  dbConnectionSource    :: !ConnectionSource
, dbTransactionSettings :: !TransactionSettings
, dbLastQuery           :: !SQL
, dbQueryResult         :: !(Maybe QueryResult)
}
