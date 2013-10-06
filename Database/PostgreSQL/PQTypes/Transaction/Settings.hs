{-# OPTIONS_GHC -Wall #-}
module Database.PostgreSQL.PQTypes.Transaction.Settings (
    TransactionSettings(..)
  , IsolationLevel(..)
  , Permissions(..)
  , defaultTransactionSettings
  ) where

data TransactionSettings = TransactionSettings {
  tsAutoTransaction :: !Bool
, tsIsolationLevel  :: !IsolationLevel
, tsPermissions     :: !Permissions
}

data IsolationLevel = DefaultLevel | ReadCommitted | RepeatableRead | Serializable
data Permissions = DefaultPermissions | ReadOnly | ReadWrite

defaultTransactionSettings :: TransactionSettings
defaultTransactionSettings = TransactionSettings {
  tsAutoTransaction = True
, tsIsolationLevel  = DefaultLevel
, tsPermissions     = DefaultPermissions
}
