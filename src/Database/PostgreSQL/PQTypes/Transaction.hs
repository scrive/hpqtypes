module Database.PostgreSQL.PQTypes.Transaction
  ( Savepoint (..)
  , withSavepoint
  , begin
  , commit
  , rollback
  , unsafeWithoutTransaction
  ) where

import Control.Monad.Catch
import Data.String
import GHC.Stack

import Data.Monoid.Utils
import Database.PostgreSQL.PQTypes.Class
import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.SQL.Raw
import Database.PostgreSQL.PQTypes.Transaction.Settings
import Database.PostgreSQL.PQTypes.Utils

-- | Wrapper that represents savepoint name.
newtype Savepoint = Savepoint (RawSQL ())

instance IsString Savepoint where
  fromString = Savepoint . fromString

-- | Create a savepoint and roll back to it if given monadic action throws.
-- This may only be used if a transaction is already active. Note that it
-- provides something like \"nested transaction\".
--
-- See <http://www.postgresql.org/docs/current/static/sql-savepoint.html>
withSavepoint :: (HasCallStack, MonadDB m, MonadMask m) => Savepoint -> m a -> m a
withSavepoint (Savepoint savepoint) m = do
  fst
    <$> generalBracket
      (runQuery_ $ "SAVEPOINT" <+> savepoint)
      ( \() -> \case
          ExitCaseSuccess _ -> runQuery_ sqlReleaseSavepoint
          _ -> rollbackAndReleaseSavepoint
      )
      (\() -> m)
  where
    sqlReleaseSavepoint = "RELEASE SAVEPOINT" <+> savepoint
    rollbackAndReleaseSavepoint = do
      runQuery_ $ "ROLLBACK TO SAVEPOINT" <+> savepoint
      runQuery_ sqlReleaseSavepoint

----------------------------------------

-- Note: sql queries in below functions that modify transaction state need to
-- not be interruptible so we don't end up in unexpected transaction
-- state. However, getConnectionAcquisitionMode should be interruptible to not
-- lead to deadlocks if a connection ends up being used from multiple threads.

-- | Begin transaction using given transaction settings.
begin :: (HasCallStack, MonadDB m, MonadMask m) => m ()
begin = do
  getConnectionAcquisitionMode >>= \case
    AcquireOnDemand -> do
      throwDB $ HPQTypesError "Can't begin a transaction in OnDemand mode"
    AcquireAndHold isolationLevel permissions -> uninterruptibleMask_ $ do
      runSQL_ $
        smconcat
          [ "BEGIN"
          , case isolationLevel of
              DefaultLevel -> ""
              ReadCommitted -> "ISOLATION LEVEL READ COMMITTED"
              RepeatableRead -> "ISOLATION LEVEL REPEATABLE READ"
              Serializable -> "ISOLATION LEVEL SERIALIZABLE"
          , case permissions of
              DefaultPermissions -> ""
              ReadOnly -> "READ ONLY"
              ReadWrite -> "READ WRITE"
          ]

-- | Commit active transaction using given transaction settings.
commit :: (HasCallStack, MonadDB m, MonadMask m) => m ()
commit = do
  getConnectionAcquisitionMode >>= \case
    AcquireOnDemand -> do
      throwDB $ HPQTypesError "Can't commit a transaction in OnDemand mode"
    AcquireAndHold {} -> uninterruptibleMask_ $ do
      runSQL_ "COMMIT"
      begin

-- | Rollback active transaction using given transaction settings.
rollback :: (HasCallStack, MonadDB m, MonadMask m) => m ()
rollback = do
  getConnectionAcquisitionMode >>= \case
    AcquireOnDemand -> do
      throwDB $ HPQTypesError "Can't rollback a transaction in OnDemand mode"
    AcquireAndHold {} -> uninterruptibleMask_ $ do
      runSQL_ "ROLLBACK"
      begin

-- | Run a block of code without an open transaction.
--
-- This function is unsafe, because if there is a transaction in progress, it's
-- commited, so the atomicity guarantee is lost.
unsafeWithoutTransaction
  :: (HasCallStack, MonadDB m, MonadMask m)
  => m a
  -> m a
unsafeWithoutTransaction action = do
  getConnectionAcquisitionMode >>= \case
    AcquireOnDemand -> action
    AcquireAndHold {} ->
      bracket_
        (uninterruptibleMask_ $ runSQL_ "COMMIT")
        begin
        action
