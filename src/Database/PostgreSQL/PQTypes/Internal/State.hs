{-# OPTIONS_GHC -Wall #-}
module Database.PostgreSQL.PQTypes.Internal.State (
    DBState(..)
  ) where

import Database.PostgreSQL.PQTypes.Internal.Connection
import Database.PostgreSQL.PQTypes.Internal.QueryResult
import Database.PostgreSQL.PQTypes.SQL.Class
import Database.PostgreSQL.PQTypes.Transaction.Settings

data DBState = DBState {
  dbConnection          :: !Connection
, dbConnectionSource    :: !ConnectionSource
, dbTransactionSettings :: !TransactionSettings
, dbLastQuery           :: !SomeSQL
, dbQueryResult         :: !(Maybe QueryResult)
}
