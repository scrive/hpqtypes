module Database.PostgreSQL.PQTypes.Utils where

import Control.Monad

import Database.PostgreSQL.PQTypes.Class
import Database.PostgreSQL.PQTypes.FromRow
import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.Internal.QueryResult
import Database.PostgreSQL.PQTypes.SQL
import Database.PostgreSQL.PQTypes.SQL.Class
import Database.PostgreSQL.PQTypes.SQL.Raw

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
runQuery01 :: (IsSQL sql, MonadDB m) => sql -> m Bool
runQuery01 sql = do
  n <- runQuery sql
  when (n > 1) $ throwDB AffectedRowsMismatch {
    rowsExpected = [(0, 1)]
  , rowsDelivered = n
  }
  return $ n == 1

-- | Specialization of 'runQuery01' that discards the result.
runQuery01_ :: (IsSQL sql, MonadDB m) => sql -> m ()
runQuery01_ = void . runQuery01

----------------------------------------

-- | Specialization of 'runQuery' to 'SQL' type.
runSQL :: MonadDB m => SQL -> m Int
runSQL = runQuery

-- | Specialization of 'runQuery_' to 'SQL' type.
runSQL_ :: MonadDB m => SQL -> m ()
runSQL_ = runQuery_

-- | Specialization of 'runQuery01' to 'SQL' type.
runSQL01 :: MonadDB m => SQL -> m Bool
runSQL01 = runQuery01

-- | Specialization of 'runQuery01_' to 'SQL' type.
runSQL01_ :: MonadDB m => SQL -> m ()
runSQL01_ = runQuery01_

----------------------------------------

-- | Specialization of 'foldrM' that fetches list of rows.
fetchMany :: (MonadDB m, FromRow row) => (row -> t) -> m [t]
fetchMany f = foldrM (\row acc -> return $ f row : acc) []

-- | Specialization of 'foldlM' that fetches one or zero rows. If more
-- rows are delivered, 'AffectedRowsMismatch' exception is thrown.
fetchMaybe :: (MonadDB m, FromRow row) => (row -> t) -> m (Maybe t)
fetchMaybe f = flip foldlM Nothing $ \acc row -> case acc of
  Just _ -> do
    Just res <- getQueryResult
    throwDB AffectedRowsMismatch {
      rowsExpected  = [(0, 1)]
    , rowsDelivered = ntuples res
    }
  Nothing -> return . Just . f $ row

-- | Specialization of 'foldlM' that fetches exactly one row. If different
-- number of rows is delivered, 'AffectedRowsMismatch' exception is thrown.
fetchOne :: (MonadDB m, FromRow row) => (row -> t) -> m t
fetchOne f = do
  mt <- fetchMaybe f
  case mt of
    Just t  -> return t
    Nothing -> throwDB AffectedRowsMismatch {
      rowsExpected = [(1, 1)]
    , rowsDelivered = 0
    }
