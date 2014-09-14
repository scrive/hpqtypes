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
  tsAutoTransaction                :: !Bool
-- | Isolation level of all transactions.
, tsIsolationLevel                 :: !IsolationLevel
-- | Defines behavior of 'withTransaction'. If set to 'Nothing',
-- no retries will be attempted on serialization failure. If set
-- to Just N, the transaction will be restarted at most N times
-- in case of serialization failures. It is up to the caller to
-- ensure that is it safe to execute monatic action supplied to
-- 'withTransaction' multiple times.
, tsRestartAfterSerializationFailure :: Maybe Int
-- | Permissions of all transactions.
, tsPermissions                    :: !Permissions
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
, tsRestartAfterSerializationFailure = Nothing
, tsPermissions     = DefaultPermissions
}
