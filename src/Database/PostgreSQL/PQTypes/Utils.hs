module Database.PostgreSQL.PQTypes.Utils (
    throwDB
  , raw
  , runQuery_
  , runQuery01
  , runQuery01OrElse
  , runQuery01_
  , runQuery01OrElse_
  , runQuery1
  , runQuery1OrElse
  , runQueryAndFetch1
  , runQueryAndFetch1OrElse
  , runQueryNot0
  , runQueryNot0OrElse
  , runQueryNot0_
  , runQueryNot0OrElse_
  , runSQL
  , runSQL_
  , runSQL01
  , runSQL01OrElse
  , runSQL01_
  , runSQL01OrElse_
  , runSQL1
  , runSQL1OrElse
  , runSQLAndFetch1
  , runSQLAndFetch1OrElse
  , runSQLNot0
  , runSQLNot0OrElse
  , runSQLNot0_
  , runSQLNot0OrElse_
  -- Internal.Utils
  , hpqTypesError
  ) where

import Control.Monad
import Control.Monad.Catch

import Database.PostgreSQL.PQTypes.Class
import Database.PostgreSQL.PQTypes.Exception
import Database.PostgreSQL.PQTypes.Fold
import Database.PostgreSQL.PQTypes.FromRow
import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.Internal.Exception
import Database.PostgreSQL.PQTypes.Internal.Utils
import Database.PostgreSQL.PQTypes.SQL
import Database.PostgreSQL.PQTypes.SQL.Class
import Database.PostgreSQL.PQTypes.SQL.Raw

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
  :: (IsSQL sql, MonadDB m, MonadThrow m)
  => (forall e. Exception e => e -> m ())
  -> sql
  -> m Bool
runQuery01OrElse f sql = do
  n <- runQuery sql
  let exc =
        AffectedRowsMismatch
          { rowsExpected = [(0, 1)]
          , rowsDelivered = n
          }
  when (n > 1) $ do
    withFrozenLastQuery $ f exc
  return $ n == 1

-- | Specialization of 'runQuery' that checks whether affected/returned
-- number of rows is in range [0, 1] and returns appropriate 'Bool' value.
-- Otherwise, 'AffectedRowsMismatch' exception is thrown.
{-# INLINABLE runQuery01 #-}
runQuery01 :: (IsSQL sql, MonadDB m, MonadThrow m) => sql -> m Bool
runQuery01 = runQuery01OrElse throwDB

-- | Specialization of 'runQuery01OrElse' that discards the result.
{-# INLINABLE runQuery01OrElse_ #-}
runQuery01OrElse_
  :: (IsSQL sql, MonadDB m, MonadThrow m)
  => (forall e. Exception e => e -> m ())
  -> sql
  -> m ()
runQuery01OrElse_ f = void . runQuery01OrElse f

-- | Specialization of 'runQuery01' that discards the result.
{-# INLINABLE runQuery01_ #-}
runQuery01_ :: (IsSQL sql, MonadDB m, MonadThrow m) => sql -> m ()
runQuery01_ = void . runQuery01

-- | Specialization of 'runQuery' that checks whether affected/returned
-- number of rows is exactly 1.
-- Otherwise, the given exception is thrown.
{-# INLINABLE runQueryAndFetch1OrElse #-}
runQueryAndFetch1OrElse
  :: (IsSQL sql, FromRow row, MonadDB m, MonadThrow m)
  => (forall e. Exception e => e -> m a)
  -> (row -> a)
  -> sql
  -> m a
runQueryAndFetch1OrElse f decoder sql = do
  n <- runQuery sql
  let exc =
        AffectedRowsMismatch
          { rowsExpected = [(1, 1)]
          , rowsDelivered = n
          }
  case n of
    0 -> withFrozenLastQuery $ f exc
    1 -> fetchOne decoder
    _ -> throwDB exc

-- | Specialization of 'runQuery' that checks whether affected/returned
-- number of rows is exactly 1.
-- Otherwise, 'AffectedRowsMismatch' exception is thrown.
{-# INLINABLE runQueryAndFetch1 #-}
runQueryAndFetch1
  :: (IsSQL sql, FromRow row, MonadDB m, MonadThrow m)
  => (row -> a)
  -> sql
  -> m a
runQueryAndFetch1 = runQueryAndFetch1OrElse throwDB

-- | Specialization of 'runQuery' that checks whether affected/returned
-- number of rows is exactly 1.
-- Otherwise, the given exception is thrown.
{-# INLINABLE runQuery1OrElse #-}
runQuery1OrElse
  :: (IsSQL sql, MonadDB m, MonadThrow m)
  => (forall e. Exception e => e -> m ())
  -> sql
  -> m ()
runQuery1OrElse f sql = do
  n <- runQuery sql
  let exc =
        AffectedRowsMismatch
          { rowsExpected = [(1, 1)]
          , rowsDelivered = n
          }
  case n of
    0 -> withFrozenLastQuery $ f exc
    1 -> pure ()
    _ -> throwDB exc

-- | Specialization of 'runQuery' that checks whether affected/returned
-- number of rows is exactly 1.
-- Otherwise, 'AffectedRowsMismatch' exception is thrown.
{-# INLINABLE runQuery1 #-}
runQuery1 :: (IsSQL sql, MonadDB m, MonadThrow m) => sql -> m ()
runQuery1 = runQuery1OrElse throwDB

-- | Specialization of 'runQuery' that checks whether affected/returned
-- number of rows is greater than 0.
-- Otherwise, the given exception is thrown.
{-# INLINABLE runQueryNot0OrElse #-}
runQueryNot0OrElse
  :: (IsSQL sql, MonadDB m, MonadThrow m)
  => (forall e. Exception e => e -> m ())
  -> sql
  -> m Int
runQueryNot0OrElse f sql = do
  n <- runQuery sql
  let exc =
        AffectedRowsMismatch
          { rowsExpected = [(1, maxBound)]
          , rowsDelivered = n
          }
  when (n == 0) $ do
    withFrozenLastQuery $ f exc
  return n

-- | Specialization of 'runQuery' that checks whether affected/returned
-- number of rows is greater than 0.
-- Otherwise, 'AffectedRowsMismatch' exception is thrown.
{-# INLINABLE runQueryNot0 #-}
runQueryNot0 :: (IsSQL sql, MonadDB m, MonadThrow m) => sql -> m Int
runQueryNot0 = runQueryNot0OrElse throwDB

{-# INLINABLE runQueryNot0OrElse_ #-}
runQueryNot0OrElse_
  :: (IsSQL sql, MonadDB m, MonadThrow m)
  => (forall e. Exception e => e -> m ())
  -> sql
  -> m ()
runQueryNot0OrElse_ f sql = void $ runQueryNot0OrElse f sql

{-# INLINABLE runQueryNot0_ #-}
runQueryNot0_ :: (IsSQL sql, MonadDB m, MonadThrow m) => sql -> m ()
runQueryNot0_ = void . runQueryNot0

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
  :: (MonadDB m, MonadThrow m)
  => (forall e. Exception e => e -> m ())
  -> SQL
  -> m Bool
runSQL01OrElse = runQuery01OrElse

-- | Specialization of 'runQuery01_' to 'SQL' type.
{-# INLINABLE runSQL01_ #-}
runSQL01_ :: (MonadDB m, MonadThrow m) => SQL -> m ()
runSQL01_ = runQuery01_

-- | Specialization of 'runQuery01OrElse_' to 'SQL' type.
{-# INLINABLE runSQL01OrElse_ #-}
runSQL01OrElse_
  :: (MonadDB m, MonadThrow m)
  => (forall e. Exception e => e -> m ())
  -> SQL
  -> m ()
runSQL01OrElse_ = runQuery01OrElse_

-- | Specialization of 'runQuery1' to 'SQL' type.
{-# INLINABLE runSQLAndFetch1 #-}
runSQLAndFetch1
  :: (FromRow row, MonadDB m, MonadThrow m) => (row -> a) -> SQL -> m a
runSQLAndFetch1 = runQueryAndFetch1

-- | Specialization of 'runQuery1OrElse' to 'SQL' type.
{-# INLINABLE runSQLAndFetch1OrElse #-}
runSQLAndFetch1OrElse
  :: (FromRow row, MonadDB m, MonadThrow m)
  => (forall e. Exception e => e -> m a)
  -> (row -> a)
  -> SQL
  -> m a
runSQLAndFetch1OrElse = runQueryAndFetch1OrElse

-- | Specialization of 'runQuery1' to 'SQL' type.
{-# INLINABLE runSQL1 #-}
runSQL1 :: (MonadDB m, MonadThrow m) => SQL -> m ()
runSQL1 = runQuery1

-- | Specialization of 'runQuery1OrElse' to 'SQL' type.
{-# INLINABLE runSQL1OrElse #-}
runSQL1OrElse
  :: (MonadDB m, MonadThrow m)
  => (forall e. Exception e => e -> m ())
  -> SQL
  -> m ()
runSQL1OrElse = runQuery1OrElse

-- | Specialization of 'runQueryNot0' to 'SQL' type.
{-# INLINABLE runSQLNot0 #-}
runSQLNot0 :: (MonadDB m, MonadThrow m) => SQL -> m Int
runSQLNot0 = runQueryNot0

-- | Specialization of 'runQueryNot0OrElse' to 'SQL' type.
{-# INLINABLE runSQLNot0OrElse #-}
runSQLNot0OrElse
  :: (MonadDB m, MonadThrow m)
  => (forall e. Exception e => e -> m ())
  -> SQL
  -> m Int
runSQLNot0OrElse = runQueryNot0OrElse

-- | Specialization of 'runQueryNot0_' to 'SQL' type.
{-# INLINABLE runSQLNot0_ #-}
runSQLNot0_ :: (MonadDB m, MonadThrow m) => SQL -> m ()
runSQLNot0_ = runQueryNot0_

-- | Specialization of 'runQueryNot0OrElse_' to 'SQL' type.
{-# INLINABLE runSQLNot0OrElse_ #-}
runSQLNot0OrElse_
  :: (MonadDB m, MonadThrow m)
  => (forall e. Exception e => e -> m ())
  -> SQL
  -> m ()
runSQLNot0OrElse_ = runQueryNot0OrElse_
