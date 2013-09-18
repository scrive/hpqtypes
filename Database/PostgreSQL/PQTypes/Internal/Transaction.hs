{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Database.PostgreSQL.PQTypes.Internal.Transaction where

import Control.Monad

import Database.PostgreSQL.PQTypes.Class
import Database.PostgreSQL.PQTypes.Internal.State
import Database.PostgreSQL.PQTypes.Internal.Utils

defaultTransactionSettings :: TransactionSettings
defaultTransactionSettings = TransactionSettings {
  tsAutoTransaction    = True
, tsIsolationLevel     = DefaultLevel
, tsPermissions        = DefaultPermissions
}

----------------------------------------

begin :: MonadDB m => m ()
begin = getTransactionSettings >>= begin'

commit :: MonadDB m => m ()
commit = getTransactionSettings >>= commit'

rollback :: MonadDB m => m ()
rollback = getTransactionSettings >>= rollback'

----------------------------------------

begin' :: MonadDB m => TransactionSettings -> m ()
begin' ts = do
  _ <- runQuery . mintercalate " " $ ["BEGIN", isolationLevel, permissions]
  return ()
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

commit' :: MonadDB m => TransactionSettings -> m ()
commit' ts = do
  _ <- runQuery "COMMIT"
  when (tsAutoTransaction ts) $
    begin' ts

rollback' :: MonadDB m => TransactionSettings -> m ()
rollback' ts = do
  _ <- runQuery "ROLLBACK"
  when (tsAutoTransaction ts) $
    begin' ts
