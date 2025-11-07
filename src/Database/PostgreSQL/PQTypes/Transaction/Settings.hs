module Database.PostgreSQL.PQTypes.Transaction.Settings
  ( RestartPredicate (..)
  , TransactionSettings (..)
  , ConnectionAcquisitionMode (..)
  , IsolationLevel (..)
  , Permissions (..)
  , defaultTransactionSettings
  ) where

import Control.Exception qualified as E

-- | Predicate that determines whether the transaction has to be restarted.
data RestartPredicate
  = forall e.
    E.Exception e =>
    RestartPredicate (e -> Integer -> Bool)

instance Show RestartPredicate where
  showsPrec _ RestartPredicate {} = (++) "RestartPredicate"

data TransactionSettings = TransactionSettings
  { tsRestartPredicate :: !(Maybe RestartPredicate)
  -- ^ Defines behavior of 'withTransaction' in case exceptions thrown within
  -- supplied monadic action are not caught and reach its body.
  --
  -- If set to 'Nothing', exceptions will be propagated as usual.
  --
  -- If set to 'Just' f, exceptions will be intercepted and passed to f along
  -- with a number that indicates how many times the transaction block already
  -- failed.
  --
  -- If f returns 'True', the transaction is restarted. Otherwise the
  -- exception is further propagated. This allows for restarting transactions
  -- e.g. in case of serialization failure. It is up to the caller to ensure
  -- that is it safe to execute supplied monadic action multiple times.
  , tsConnectionAcquisitionMode :: !ConnectionAcquisitionMode
  -- ^ Acquisition mode of a database connection.
  }
  deriving (Show)

data IsolationLevel = DefaultLevel | ReadCommitted | RepeatableRead | Serializable
  deriving (Eq, Ord, Show)

data Permissions = DefaultPermissions | ReadOnly | ReadWrite
  deriving (Eq, Ord, Show)

-- | Acquisition mode of a database connection.
data ConnectionAcquisitionMode
  = -- | Acquire a connection on demand, i.e. only when a query needs to be
    -- run. This mode enables you to have a
    -- t'Database.PostgreSQL.PQTypes.Class.MonadDB' constraint in scope without
    -- keeping a transaction open, but is limited to executionn of read only
    -- queries.
    AcquireOnDemand
  | -- | Acquire a connection, start a transaction with a given isolation level
    -- and permissions and hold onto it for the duration of a
    -- t'Database.PostgreSQL.PQTypes.Class.MonadDB' constraint in scope.
    AcquireAndHold !IsolationLevel !Permissions
  deriving (Eq, Ord, Show)

-- | Default transaction settings.
defaultTransactionSettings :: TransactionSettings
defaultTransactionSettings =
  TransactionSettings
    { tsRestartPredicate = Nothing
    , tsConnectionAcquisitionMode = AcquireAndHold DefaultLevel DefaultPermissions
    }
