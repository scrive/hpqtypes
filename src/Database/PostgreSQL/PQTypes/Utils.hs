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
runQuery_ :: (IsSQL sql, MonadDB m) => sql -> m ()
runQuery_ = void . runQuery

-- | Specialization of 'runQuery' that checks whether affected/returned
-- number of rows is in range [0, 1] and returns appropriate 'Bool' value.
-- Otherwise, 'AffectedRowsMismatch' exception is thrown.
runQuery01 :: (IsSQL sql, MonadDB m, MonadThrow m) => sql -> m Bool
runQuery01 sql = do
  n <- runQuery sql
  when (n > 1) $ throwDB AffectedRowsMismatch {
    rowsExpected = [(0, 1)]
  , rowsDelivered = n
  }
  return $ n == 1

-- | Specialization of 'runQuery01' that discards the result.
runQuery01_ :: (IsSQL sql, MonadDB m, MonadThrow m) => sql -> m ()
runQuery01_ = void . runQuery01

----------------------------------------

-- | Specialization of 'runQuery' to 'SQL' type.
runSQL :: MonadDB m => SQL -> m Int
runSQL = runQuery

-- | Specialization of 'runQuery_' to 'SQL' type.
runSQL_ :: MonadDB m => SQL -> m ()
runSQL_ = runQuery_

-- | Specialization of 'runQuery01' to 'SQL' type.
runSQL01 :: (MonadDB m, MonadThrow m) => SQL -> m Bool
runSQL01 = runQuery01

-- | Specialization of 'runQuery01_' to 'SQL' type.
runSQL01_ :: (MonadDB m, MonadThrow m) => SQL -> m ()
runSQL01_ = runQuery01_

----------------------------------------
