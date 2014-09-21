{-# LANGUAGE DeriveDataTypeable, GADTs, Rank2Types
  , RecordWildCards #-}
module Database.PostgreSQL.PQTypes.Transaction.Settings (
    RestartPredicate(..)
  , TransactionSettings(..)
  , IsolationLevel(..)
  , Permissions(..)
  , defaultTransactionSettings
  ) where

import Data.Typeable
import qualified Control.Exception as E

-- | Predicate that determines whether the transaction has to be restarted.
data RestartPredicate where
  RestartPredicate   :: forall e. E.Exception e
                     => (e -> Integer -> Bool) -> RestartPredicate
  NoRestartPredicate :: RestartPredicate
  deriving Typeable

instance Show RestartPredicate where
  showsPrec _ RestartPredicate{} = (++) "RestartPredicate"
  showsPrec _ NoRestartPredicate = (++) "NoRestartPredicate"

data TransactionSettings = TransactionSettings {
-- | If set to True, transaction will be automatically started at the
-- beginning of database action and after each 'commit' / 'rollback'.
-- If set to False, no transaction will automatically start in either
-- of above cases.
  tsAutoTransaction  :: !Bool
-- | Isolation level of all transactions.
, tsIsolationLevel   :: !IsolationLevel
-- | Defines behavior of 'withTransaction' in case exceptions thrown
-- within supplied monadic action are not caught and reach its body.
-- If set to 'Nothing', exceptions will be propagated as usual. If
-- set to 'Just' f, exceptions will be intercepted and passed to f
-- along with a number that indicates how many times the transaction
-- block already failed. If f returns 'True', the transaction is
-- restarted. Otherwise the exception is further propagated. This
-- allows for restarting transactions e.g. in case of serialization
-- failure. It is up to the caller to ensure that is it safe to
-- execute supplied monadic action multiple times.
, tsRestartPredicate :: !RestartPredicate
-- | Permissions of all transactions.
, tsPermissions      :: !Permissions
} deriving (Show, Typeable)

data IsolationLevel = DefaultLevel | ReadCommitted | RepeatableRead | Serializable
  deriving (Eq, Ord, Show, Typeable)

data Permissions = DefaultPermissions | ReadOnly | ReadWrite
  deriving (Eq, Ord, Show, Typeable)

-- | Default transaction settings.
defaultTransactionSettings :: TransactionSettings
defaultTransactionSettings = TransactionSettings {
  tsAutoTransaction  = True
, tsIsolationLevel   = DefaultLevel
, tsRestartPredicate = NoRestartPredicate
, tsPermissions      = DefaultPermissions
}
