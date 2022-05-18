module Database.PostgreSQL.PQTypes.Utils (
    throwDB
  , raw
  , runQuery_
  , runQuery01
  , runQuery01_
  , runSQL
  , runSQL_
  , runSQL01
  , runSQL01_
  , runPreparedQuery_
  , runPreparedQuery01
  , runPreparedQuery01_
  , runPreparedSQL
  , runPreparedSQL_
  , runPreparedSQL01
  , runPreparedSQL01_
  -- Internal.Utils
  , hpqTypesError
  ) where

import Control.Monad
import Control.Monad.Catch

import Database.PostgreSQL.PQTypes.Class
import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.Internal.Exception
import Database.PostgreSQL.PQTypes.Internal.Query
import Database.PostgreSQL.PQTypes.Internal.Utils
import Database.PostgreSQL.PQTypes.SQL
import Database.PostgreSQL.PQTypes.SQL.Class
import Database.PostgreSQL.PQTypes.SQL.Raw

-- | When given 'DBException', throw it immediately. Otherwise
-- wrap it in 'DBException' with the current query context first.
{-# INLINABLE throwDB #-}
throwDB :: (Exception e, MonadDB m, MonadThrow m) => e -> m a
throwDB e = case fromException $ toException e of
  Just (dbe::DBException) -> throwM dbe
  Nothing -> do
    SomeSQL sql <- getLastQuery
    throwM DBException {
      dbeQueryContext = sql
    , dbeError = e
    }

----------------------------------------

-- | Convert 'RawSQL' () to 'SQL'.
raw :: RawSQL () -> SQL
raw = mkSQL . unRawSQL

----------------------------------------

-- | Specialization of 'runQuery' that discards the result.
{-# INLINABLE runQuery_ #-}
runQuery_ :: (IsSQL sql, MonadDB m) => sql -> m ()
runQuery_ = void . runQuery

-- | Specialization of 'runQuery' that checks whether affected/returned
-- number of rows is in range [0, 1] and returns appropriate 'Bool' value.
-- Otherwise, 'AffectedRowsMismatch' exception is thrown.
{-# INLINABLE runQuery01 #-}
runQuery01 :: (IsSQL sql, MonadDB m, MonadThrow m) => sql -> m Bool
runQuery01 sql = do
  n <- runQuery sql
  when (n > 1) $ throwDB AffectedRowsMismatch {
    rowsExpected = [(0, 1)]
  , rowsDelivered = n
  }
  return $ n == 1

-- | Specialization of 'runQuery01' that discards the result.
{-# INLINABLE runQuery01_ #-}
runQuery01_ :: (IsSQL sql, MonadDB m, MonadThrow m) => sql -> m ()
runQuery01_ = void . runQuery01

----------------------------------------

-- | Specialization of 'runQuery' to 'SQL' type.
{-# INLINABLE runSQL #-}
runSQL :: MonadDB m => SQL -> m Int
runSQL = runQuery

-- | Specialization of 'runQuery_' to 'SQL' type.
{-# INLINABLE runSQL_ #-}
runSQL_ :: MonadDB m => SQL -> m ()
runSQL_ = runQuery_

-- | Specialization of 'runQuery01' to 'SQL' type.
{-# INLINABLE runSQL01 #-}
runSQL01 :: (MonadDB m, MonadThrow m) => SQL -> m Bool
runSQL01 = runQuery01

-- | Specialization of 'runQuery01_' to 'SQL' type.
{-# INLINABLE runSQL01_ #-}
runSQL01_ :: (MonadDB m, MonadThrow m) => SQL -> m ()
runSQL01_ = runQuery01_

----------------------------------------

-- | Specialization of 'runPreparedQuery' that discards the result.
{-# INLINABLE runPreparedQuery_ #-}
runPreparedQuery_ :: (IsSQL sql, MonadDB m) => QueryName -> sql -> m ()
runPreparedQuery_ name = void . runPreparedQuery name

-- | Specialization of 'runPreparedQuery' that checks whether affected/returned
-- number of rows is in range [0, 1] and returns appropriate 'Bool' value.
-- Otherwise, 'AffectedRowsMismatch' exception is thrown.
{-# INLINABLE runPreparedQuery01 #-}
runPreparedQuery01 :: (IsSQL sql, MonadDB m, MonadThrow m) => QueryName -> sql -> m Bool
runPreparedQuery01 name sql = do
  n <- runPreparedQuery name sql
  when (n > 1) $ throwDB AffectedRowsMismatch {
    rowsExpected = [(0, 1)]
  , rowsDelivered = n
  }
  return $ n == 1

-- | Specialization of 'runPreparedQuery01' that discards the result.
{-# INLINABLE runPreparedQuery01_ #-}
runPreparedQuery01_ :: (IsSQL sql, MonadDB m, MonadThrow m) => QueryName -> sql -> m ()
runPreparedQuery01_ name = void . runPreparedQuery01 name

----------------------------------------

-- | Specialization of 'runPreparedQuery' to 'SQL' type.
{-# INLINABLE runPreparedSQL #-}
runPreparedSQL :: MonadDB m => QueryName -> SQL -> m Int
runPreparedSQL = runPreparedQuery

-- | Specialization of 'runPreparedQuery_' to 'SQL' type.
{-# INLINABLE runPreparedSQL_ #-}
runPreparedSQL_ :: MonadDB m => QueryName -> SQL -> m ()
runPreparedSQL_ = runPreparedQuery_

-- | Specialization of 'runPreparedQuery01' to 'SQL' type.
{-# INLINABLE runPreparedSQL01 #-}
runPreparedSQL01 :: (MonadDB m, MonadThrow m) => QueryName -> SQL -> m Bool
runPreparedSQL01 = runPreparedQuery01

-- | Specialization of 'runPreparedQuery01_' to 'SQL' type.
{-# INLINABLE runPreparedSQL01_ #-}
runPreparedSQL01_ :: (MonadDB m, MonadThrow m) => QueryName -> SQL -> m ()
runPreparedSQL01_ = runPreparedQuery01_
