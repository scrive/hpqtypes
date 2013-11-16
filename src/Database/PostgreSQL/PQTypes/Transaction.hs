{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings, RecordWildCards #-}
module Database.PostgreSQL.PQTypes.Transaction (
    withTransaction
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

import Data.Monoid.Utils
import Database.PostgreSQL.PQTypes.Class
import Database.PostgreSQL.PQTypes.Transaction.Settings
import Database.PostgreSQL.PQTypes.Utils

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

commit' :: MonadDB m => TransactionSettings -> m ()
commit' ts = do
  runSQL_ "COMMIT"
  when (tsAutoTransaction ts) $
    begin' ts

rollback' :: MonadDB m => TransactionSettings -> m ()
rollback' ts = do
  runSQL_ "ROLLBACK"
  when (tsAutoTransaction ts) $
    begin' ts
