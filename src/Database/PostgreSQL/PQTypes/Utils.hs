module Database.PostgreSQL.PQTypes.Utils
  ( throwDB
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
import GHC.Stack

import Database.PostgreSQL.PQTypes.Class
import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.Internal.Exception
import Database.PostgreSQL.PQTypes.Internal.Utils
import Database.PostgreSQL.PQTypes.SQL
import Database.PostgreSQL.PQTypes.SQL.Class
import Database.PostgreSQL.PQTypes.SQL.Raw

-- | When given 'DBException', throw it immediately. Otherwise
-- wrap it in 'DBException' with the current query context first.
throwDB :: (HasCallStack, Exception e, MonadDB m, MonadThrow m) => e -> m a
throwDB e = case fromException $ toException e of
  Just (dbe :: DBException) -> throwM dbe
  Nothing -> do
    SomeSQL sql <- getLastQuery
    pid <- getBackendPid
    throwM
      DBException
        { dbeQueryContext = sql
        , dbeBackendPid = pid
        , dbeError = e
        , dbeCallStack = callStack
        }

----------------------------------------

-- | Convert 'RawSQL' () to 'SQL'.
raw :: RawSQL () -> SQL
raw = mkSQL . unRawSQL

----------------------------------------

-- | Specialization of 'runQuery' that discards the result.
runQuery_ :: (HasCallStack, IsSQL sql, MonadDB m) => sql -> m ()
runQuery_ = withFrozenCallStack $ void . runQuery

-- | Specialization of 'runQuery' that checks whether affected/returned
-- number of rows is in range [0, 1] and returns appropriate 'Bool' value.
-- Otherwise, 'AffectedRowsMismatch' exception is thrown.
runQuery01 :: (HasCallStack, IsSQL sql, MonadDB m, MonadThrow m) => sql -> m Bool
runQuery01 sql = withFrozenCallStack $ do
  n <- runQuery sql
  when (n > 1) $
    throwDB
      AffectedRowsMismatch
        { rowsExpected = [(0, 1)]
        , rowsDelivered = n
        }
  return $ n == 1

-- | Specialization of 'runQuery01' that discards the result.
runQuery01_ :: (HasCallStack, IsSQL sql, MonadDB m, MonadThrow m) => sql -> m ()
runQuery01_ = withFrozenCallStack $ void . runQuery01

----------------------------------------

-- | Specialization of 'runQuery' to 'SQL' type.
runSQL :: (HasCallStack, MonadDB m) => SQL -> m Int
runSQL = withFrozenCallStack runQuery

-- | Specialization of 'runQuery_' to 'SQL' type.
runSQL_ :: (HasCallStack, MonadDB m) => SQL -> m ()
runSQL_ = withFrozenCallStack runQuery_

-- | Specialization of 'runQuery01' to 'SQL' type.
runSQL01 :: (HasCallStack, MonadDB m, MonadThrow m) => SQL -> m Bool
runSQL01 = withFrozenCallStack runQuery01

-- | Specialization of 'runQuery01_' to 'SQL' type.
runSQL01_ :: (HasCallStack, MonadDB m, MonadThrow m) => SQL -> m ()
runSQL01_ = withFrozenCallStack runQuery01_

----------------------------------------

-- | Specialization of 'runPreparedQuery' that discards the result.
runPreparedQuery_ :: (HasCallStack, IsSQL sql, MonadDB m) => QueryName -> sql -> m ()
runPreparedQuery_ name = withFrozenCallStack $ void . runPreparedQuery name

-- | Specialization of 'runPreparedQuery' that checks whether affected/returned
-- number of rows is in range [0, 1] and returns appropriate 'Bool' value.
-- Otherwise, 'AffectedRowsMismatch' exception is thrown.
runPreparedQuery01
  :: (HasCallStack, IsSQL sql, MonadDB m, MonadThrow m)
  => QueryName
  -> sql
  -> m Bool
runPreparedQuery01 name sql = withFrozenCallStack $ do
  n <- runPreparedQuery name sql
  when (n > 1) $
    throwDB
      AffectedRowsMismatch
        { rowsExpected = [(0, 1)]
        , rowsDelivered = n
        }
  return $ n == 1

-- | Specialization of 'runPreparedQuery01' that discards the result.
runPreparedQuery01_
  :: (HasCallStack, IsSQL sql, MonadDB m, MonadThrow m)
  => QueryName
  -> sql
  -> m ()
runPreparedQuery01_ name = withFrozenCallStack $ void . runPreparedQuery01 name

----------------------------------------

-- | Specialization of 'runPreparedQuery' to 'SQL' type.
runPreparedSQL :: (HasCallStack, MonadDB m) => QueryName -> SQL -> m Int
runPreparedSQL = withFrozenCallStack runPreparedQuery

-- | Specialization of 'runPreparedQuery_' to 'SQL' type.
runPreparedSQL_ :: (HasCallStack, MonadDB m) => QueryName -> SQL -> m ()
runPreparedSQL_ = withFrozenCallStack runPreparedQuery_

-- | Specialization of 'runPreparedQuery01' to 'SQL' type.
runPreparedSQL01 :: (HasCallStack, MonadDB m, MonadThrow m) => QueryName -> SQL -> m Bool
runPreparedSQL01 = withFrozenCallStack runPreparedQuery01

-- | Specialization of 'runPreparedQuery01_' to 'SQL' type.
runPreparedSQL01_ :: (HasCallStack, MonadDB m, MonadThrow m) => QueryName -> SQL -> m ()
runPreparedSQL01_ = withFrozenCallStack runPreparedQuery01_
