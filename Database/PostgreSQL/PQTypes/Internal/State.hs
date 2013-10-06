{-# OPTIONS_GHC -Wall #-}
module Database.PostgreSQL.PQTypes.Internal.State (
    TransactionSettings(..)
  , IsolationLevel(..)
  , Permissions(..)
  , DBState(..)
  ) where

import Database.PostgreSQL.PQTypes.Internal.Connection
import Database.PostgreSQL.PQTypes.Internal.QueryResult
import Database.PostgreSQL.PQTypes.SQL.Class

data TransactionSettings = TransactionSettings {
  tsAutoTransaction :: !Bool
, tsIsolationLevel  :: !IsolationLevel
, tsPermissions     :: !Permissions
}

data IsolationLevel = DefaultLevel | ReadCommitted | RepeatableRead | Serializable
data Permissions = DefaultPermissions | ReadOnly | ReadWrite

----------------------------------------

data DBState = DBState {
  dbConnection          :: !Connection
, dbConnectionSource    :: !ConnectionSource
, dbTransactionSettings :: !TransactionSettings
, dbLastQuery           :: !SomeSQL
, dbQueryResult         :: !(Maybe QueryResult)
}
