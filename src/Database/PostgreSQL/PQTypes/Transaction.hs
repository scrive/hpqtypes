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
import Control.Monad.Catch
import Data.Function
import Data.String
import Data.Typeable

import Data.Monoid.Utils
import Database.PostgreSQL.PQTypes.Class
import Database.PostgreSQL.PQTypes.Internal.Exception
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
withSavepoint :: (MonadDB m, MonadMask m) => Savepoint -> m a -> m a
withSavepoint (Savepoint savepoint) m = fst <$> generalBracket
  (runQuery_ $ "SAVEPOINT" <+> savepoint)
  (\() -> \case
      ExitCaseSuccess _ -> runQuery_ sqlReleaseSavepoint
      _                 -> rollbackAndReleaseSavepoint
  )
  (\() -> m)
  where
    sqlReleaseSavepoint = "RELEASE SAVEPOINT" <+> savepoint
    rollbackAndReleaseSavepoint = do
      runQuery_ $ "ROLLBACK TO SAVEPOINT" <+> savepoint
      runQuery_ sqlReleaseSavepoint

----------------------------------------

-- | Same as 'withTransaction'' except that it uses current
-- transaction settings instead of custom ones.  It is worth
-- noting that changing transaction settings inside supplied
-- monadic action won't have any effect  on the final 'commit'
-- / 'rollback' as settings that were in effect during the call
-- to 'withTransaction' will be used.
withTransaction :: (MonadDB m, MonadMask m) => m a -> m a
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
withTransaction' :: (MonadDB m, MonadMask m)
                 => TransactionSettings -> m a -> m a
withTransaction' ts m = (`fix` 1) $ \loop n -> do
  -- Optimization for squashing possible space leaks.
  -- It looks like GHC doesn't like 'catch' and passes
  -- on introducing strictness in some cases.
  let maybeRestart = case tsRestartPredicate ts of
        Just _  -> handleJust (expred n) (\_ -> loop $ n+1)
        Nothing -> id
  maybeRestart $ fst <$> generalBracket
    (begin' ts)
    (\() -> \case
        ExitCaseSuccess _ -> commit' ts
        _                 -> rollback' ts
    )
    (\() -> m)
  where
    expred :: Integer -> SomeException -> Maybe ()
    expred !n e = do
      -- check if the predicate exists
      RestartPredicate f <- tsRestartPredicate ts
      -- cast exception to the type expected by the predicate
      err <- msum [
          -- either cast the exception itself...
          fromException e
          -- ...or extract it from DBException
        , fromException e >>= \DBException{..} -> cast dbeError
        ]
      -- check if the predicate allows for the restart
      guard $ f err n

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
