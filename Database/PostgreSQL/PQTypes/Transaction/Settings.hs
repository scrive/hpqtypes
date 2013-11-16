{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Database.PostgreSQL.PQTypes.Transaction.Settings (
    TransactionSettings(..)
  , IsolationLevel(..)
  , Permissions(..)
  , defaultTransactionSettings
  ) where

import Data.Typeable

data TransactionSettings = TransactionSettings {
  tsAutoTransaction :: !Bool
, tsIsolationLevel  :: !IsolationLevel
, tsPermissions     :: !Permissions
} deriving (Eq, Ord, Show, Typeable)

data IsolationLevel = DefaultLevel | ReadCommitted | RepeatableRead | Serializable
  deriving (Eq, Ord, Show, Typeable)

data Permissions = DefaultPermissions | ReadOnly | ReadWrite
  deriving (Eq, Ord, Show, Typeable)

defaultTransactionSettings :: TransactionSettings
defaultTransactionSettings = TransactionSettings {
  tsAutoTransaction = True
, tsIsolationLevel  = DefaultLevel
, tsPermissions     = DefaultPermissions
}
