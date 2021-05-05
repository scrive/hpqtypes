module Database.PostgreSQL.PQTypes.Utils (
    throwDB
  , raw
  , runQuery_
  , runQuery01
  , runQuery01OrElse
  , runQuery01_
  , runQuery01OrElse_
  , runQuery1_
  , runQuery1OrElse_
  , runQueryNot0_
  , runQueryNot0OrElse_
  , runSQL
  , runSQL_
  , runSQL01
  , runSQL01OrElse
  , runSQL01_
  , runSQL01OrElse_
  , runSQL1_
  , runSQL1OrElse_
  , runSQLNot0_
  , runSQLNot0OrElse_
  -- Internal.Utils
  , hpqTypesError
  ) where

import Control.Monad
import Control.Monad.Catch

import Database.PostgreSQL.PQTypes.Class
import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.Internal.Exception
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
-- Otherwise, the given exception is thrown.
{-# INLINABLE runQuery01OrElse #-}
runQuery01OrElse
  :: (Exception e, IsSQL sql, MonadDB m, MonadThrow m) => (Int -> e) -> sql -> m Bool
runQuery01OrElse f sql = do
  n <- runQuery sql
  when (n > 1) $ throwDB (f n)
  return $ n == 1

-- | Specialization of 'runQuery' that checks whether affected/returned
-- number of rows is in range [0, 1] and returns appropriate 'Bool' value.
-- Otherwise, 'AffectedRowsMismatch' exception is thrown.
{-# INLINABLE runQuery01 #-}
runQuery01 :: (IsSQL sql, MonadDB m, MonadThrow m) => sql -> m Bool
runQuery01 = runQuery01OrElse $ \n ->
  AffectedRowsMismatch {
    rowsExpected = [(0, 1)]
  , rowsDelivered = n
  }

-- | Specialization of 'runQuery01OrElse' that discards the result.
{-# INLINABLE runQuery01OrElse_ #-}
runQuery01OrElse_
  :: (Exception e, IsSQL sql, MonadDB m, MonadThrow m) => (Int -> e) -> sql -> m ()
runQuery01OrElse_ f = void . runQuery01OrElse f

-- | Specialization of 'runQuery01' that discards the result.
{-# INLINABLE runQuery01_ #-}
runQuery01_ :: (IsSQL sql, MonadDB m, MonadThrow m) => sql -> m ()
runQuery01_ = void . runQuery01

-- | Specialization of 'runQuery' that checks whether affected/returned
-- number of rows is exactly 1.
-- Otherwise, the given exception is thrown.
{-# INLINABLE runQuery1OrElse_ #-}
runQuery1OrElse_
  :: (Exception e, IsSQL sql, MonadDB m, MonadThrow m) => (Int -> e) -> sql -> m ()
runQuery1OrElse_ f sql = do
  n <- runQuery sql
  when (n /= 1) $ throwDB (f n)

-- | Specialization of 'runQuery' that checks whether affected/returned
-- number of rows is exactly 1.
-- Otherwise, 'AffectedRowsMismatch' exception is thrown.
{-# INLINABLE runQuery1_ #-}
runQuery1_ :: (IsSQL sql, MonadDB m, MonadThrow m) => sql -> m ()
runQuery1_ = runQuery1OrElse_ $ \n ->
  AffectedRowsMismatch {
    rowsExpected = [(1, 1)]
  , rowsDelivered = n
  }

-- | Specialization of 'runQuery' that checks whether affected/returned
-- number of rows is greater than 0.
-- Otherwise, the given exception is thrown.
{-# INLINABLE runQueryNot0OrElse_ #-}
runQueryNot0OrElse_
  :: (Exception e, IsSQL sql, MonadDB m, MonadThrow m) => (Int -> e) -> sql -> m ()
runQueryNot0OrElse_ f sql = do
  n <- runQuery sql
  when (n == 0) $ throwDB (f n)

-- | Specialization of 'runQuery' that checks whether affected/returned
-- number of rows is greater than 0.
-- Otherwise, 'AffectedRowsMismatch' exception is thrown.
{-# INLINABLE runQueryNot0_ #-}
runQueryNot0_ :: (IsSQL sql, MonadDB m, MonadThrow m) => sql -> m ()
runQueryNot0_ = runQueryNot0OrElse_ $ \n ->
  AffectedRowsMismatch {
    rowsExpected = [(1, maxBound)]
  , rowsDelivered = 0
  }

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

-- | Specialization of 'runQuery01OrElse' to 'SQL' type.
{-# INLINABLE runSQL01OrElse #-}
runSQL01OrElse
  :: (Exception e, MonadDB m, MonadThrow m) => (Int -> e) -> SQL -> m Bool
runSQL01OrElse = runQuery01OrElse

-- | Specialization of 'runQuery01_' to 'SQL' type.
{-# INLINABLE runSQL01_ #-}
runSQL01_ :: (MonadDB m, MonadThrow m) => SQL -> m ()
runSQL01_ = runQuery01_

-- | Specialization of 'runQuery01OrElse_' to 'SQL' type.
{-# INLINABLE runSQL01OrElse_ #-}
runSQL01OrElse_
  :: (Exception e, MonadDB m, MonadThrow m) => (Int -> e) -> SQL -> m ()
runSQL01OrElse_ = runQuery01OrElse_

-- | Specialization of 'runQuery1_' to 'SQL' type.
{-# INLINABLE runSQL1_ #-}
runSQL1_ :: (MonadDB m, MonadThrow m) => SQL -> m ()
runSQL1_ = runQuery1_

-- | Specialization of 'runQuery1OrElse_' to 'SQL' type.
{-# INLINABLE runSQL1OrElse_ #-}
runSQL1OrElse_
  :: (Exception e, MonadDB m, MonadThrow m) => (Int -> e) -> SQL -> m ()
runSQL1OrElse_ = runQuery1OrElse_

-- | Specialization of 'runQueryNot0_' to 'SQL' type.
{-# INLINABLE runSQLNot0_ #-}
runSQLNot0_ :: (MonadDB m, MonadThrow m) => SQL -> m ()
runSQLNot0_ = runQuery01_

-- | Specialization of 'runQueryNot0OrElse_' to 'SQL' type.
{-# INLINABLE runSQLNot0OrElse_ #-}
runSQLNot0OrElse_
  :: (Exception e, MonadDB m, MonadThrow m) => (Int -> e) -> SQL -> m ()
runSQLNot0OrElse_ = runQuery01OrElse_
