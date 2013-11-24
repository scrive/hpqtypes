{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings, RecordWildCards #-}
module Database.PostgreSQL.PQTypes.Transaction (
    Savepoint(..)
  , withSavepoint
  , withTransaction
  , begin
  , commit
  , rollback
  , withTransaction'
  , begin'
  , commit'
  , rollback'
  ) where

import Control.Monad
import Control.Monad.Trans.Control
import qualified Control.Exception.Lifted as LE

import Data.Monoid.Space
import Data.Monoid.Utils
import Database.PostgreSQL.PQTypes.Class
import Database.PostgreSQL.PQTypes.SQL.Raw
import Database.PostgreSQL.PQTypes.Transaction.Settings
import Database.PostgreSQL.PQTypes.Utils

-- | Wrapper that represents savepoint name.
newtype Savepoint = Savepoint (RawSQL ())

-- | Create a savepoint and roll back to it if given monadic action throws.
-- This may only be used if a transaction is already active. Note that it
-- provides something like \"nested transaction\".
--
-- See <http://www.postgresql.org/docs/current/static/sql-savepoint.html>
withSavepoint :: (MonadBaseControl IO m, MonadDB m) => Savepoint -> m a -> m a
withSavepoint (Savepoint savepoint) m = LE.mask $ \restore -> do
  runQuery_ $ "SAVEPOINT" <+> savepoint
  res <- restore m `LE.onException` rollbackAndReleaseSavepoint
  runQuery_ sqlReleaseSavepoint
  return res
  where
    sqlReleaseSavepoint = "RELEASE SAVEPOINT" <+> savepoint
    rollbackAndReleaseSavepoint = do
      runQuery_ $ "ROLLBACK TO SAVEPOINT" <+> savepoint
      runQuery_ sqlReleaseSavepoint

----------------------------------------

-- | Same as 'withTransaction'' except that it uses
-- current transaction settings instead of custom ones.
withTransaction :: (MonadBaseControl IO m, MonadDB m) => m a -> m a
withTransaction m = getTransactionSettings >>= flip withTransaction' m

-- | Begin transaction using current transaction settings.
begin :: MonadDB m => m ()
begin = getTransactionSettings >>= begin'

-- | Commit active transaction using current transaction settings.
commit :: MonadDB m => m ()
commit = getTransactionSettings >>= commit'

-- | Rollback active transaction using current transaction settings.
rollback :: MonadDB m => m ()
rollback = getTransactionSettings >>= rollback'

----------------------------------------

-- | Execute monadic action within a transaction using given transaction
-- settings. Note that it won't work as expected if a transaction is already
-- active (in such case 'withSavepoint' should be used instead).
withTransaction' :: (MonadBaseControl IO m, MonadDB m)
                 => TransactionSettings -> m a -> m a
withTransaction' ts m = LE.mask $ \restore -> do
  begin' ts
  res <- restore m `LE.onException` rollback' ts
  commit' ts
  return res

-- | Begin transaction using given transaction settings.
begin' :: MonadDB m => TransactionSettings -> m ()
begin' ts = runSQL_ . mintercalate " " $ ["BEGIN", isolationLevel, permissions]
  where
    isolationLevel = case tsIsolationLevel ts of
      DefaultLevel   -> ""
      ReadCommitted  -> "ISOLATION LEVEL READ COMMITTED"
      RepeatableRead -> "ISOLATION LEVEL REPEATABLE READ"
      Serializable   -> "ISOLATION LEVEL SERIALIZABLE"
    permissions = case tsPermissions ts of
      DefaultPermissions -> ""
      ReadOnly           -> "READ ONLY"
      ReadWrite          -> "READ WRITE"

-- | Commit active transaction using given transaction settings.
commit' :: MonadDB m => TransactionSettings -> m ()
commit' ts = do
  runSQL_ "COMMIT"
  when (tsAutoTransaction ts) $
    begin' ts

-- | Rollback active transaction using given transaction settings.
rollback' :: MonadDB m => TransactionSettings -> m ()
rollback' ts = do
  runSQL_ "ROLLBACK"
  when (tsAutoTransaction ts) $
    begin' ts
