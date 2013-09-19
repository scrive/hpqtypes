{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings, RecordWildCards #-}
module Database.PostgreSQL.PQTypes.Internal.Transaction (
    defaultTransactionSettings
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

import Database.PostgreSQL.PQTypes.Class
import Database.PostgreSQL.PQTypes.Internal.State
import Database.PostgreSQL.PQTypes.Internal.Utils

defaultTransactionSettings :: TransactionSettings
defaultTransactionSettings = TransactionSettings {
  tsAutoTransaction = True
, tsIsolationLevel  = DefaultLevel
, tsPermissions     = DefaultPermissions
}

----------------------------------------

withTransaction :: (MonadBaseControl IO m, MonadDB m) => m a -> m a
withTransaction m = getTransactionSettings >>= flip withTransaction' m

begin :: MonadDB m => m ()
begin = getTransactionSettings >>= begin'

commit :: MonadDB m => m ()
commit = getTransactionSettings >>= commit'

rollback :: MonadDB m => m ()
rollback = getTransactionSettings >>= rollback'

----------------------------------------

withTransaction' :: (MonadBaseControl IO m, MonadDB m)
                 => TransactionSettings -> m a -> m a
withTransaction' ts m = LE.mask $ \restore -> do
  begin' ts
  res <- restore m `LE.onException` rollback' ts
  commit' ts
  return res

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
