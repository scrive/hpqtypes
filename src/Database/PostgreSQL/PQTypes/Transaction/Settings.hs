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
-- | If set to True, transaction will be automatically started at the
-- beginning of database action and after each 'commit' / 'rollback'.
-- If set to False, no transaction will automatically start in either
-- of above cases.
  tsAutoTransaction :: !Bool
-- | Isolation level of all transactions.
, tsIsolationLevel  :: !IsolationLevel
-- | Permissions of all transactions.
, tsPermissions     :: !Permissions
} deriving (Eq, Ord, Show, Typeable)

data IsolationLevel = DefaultLevel | ReadCommitted | RepeatableRead | Serializable
  deriving (Eq, Ord, Show, Typeable)

data Permissions = DefaultPermissions | ReadOnly | ReadWrite
  deriving (Eq, Ord, Show, Typeable)

-- | Default transaction settings.
defaultTransactionSettings :: TransactionSettings
defaultTransactionSettings = TransactionSettings {
  tsAutoTransaction = True
, tsIsolationLevel  = DefaultLevel
, tsPermissions     = DefaultPermissions
}
