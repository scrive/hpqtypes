{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Database.PostgreSQL.PQTypes.Internal.Transaction where

import Control.Monad

import Database.PostgreSQL.PQTypes.Class
import Database.PostgreSQL.PQTypes.Internal.State
import Database.PostgreSQL.PQTypes.Internal.Utils

defaultTransactionMode :: TransactionMode
defaultTransactionMode = TransactionMode {
  tmAutoTransaction = True
, tmIsolationLevel = DefaultLevel
, tmPermissions = DefaultPermissions
}

----------------------------------------

begin :: MonadDB m => m ()
begin = getTransactionMode >>= begin'

commit :: MonadDB m => m ()
commit = getTransactionMode >>= commit'

rollback :: MonadDB m => m ()
rollback = getTransactionMode >>= rollback'

----------------------------------------

begin' :: MonadDB m => TransactionMode -> m ()
begin' tm = do
  _ <- runQuery . mintercalate " " $ ["BEGIN", isolationLevel, permissions]
  return ()
  where
    isolationLevel = case tmIsolationLevel tm of
      DefaultLevel   -> ""
      ReadCommitted  -> "ISOLATION LEVEL READ COMMITTED"
      RepeatableRead -> "ISOLATION LEVEL REPEATABLE READ"
      Serializable   -> "ISOLATION LEVEL SERIALIZABLE"
    permissions = case tmPermissions tm of
      DefaultPermissions -> ""
      ReadOnly           -> "READ ONLY"
      ReadWrite          -> "READ WRITE"

commit' :: MonadDB m => TransactionMode -> m ()
commit' tm = do
  _ <- runQuery "COMMIT"
  when (tmAutoTransaction tm) $
    begin' tm

rollback' :: MonadDB m => TransactionMode -> m ()
rollback' tm = do
  _ <- runQuery "ROLLBACK"
  when (tmAutoTransaction tm) $
    begin' tm
