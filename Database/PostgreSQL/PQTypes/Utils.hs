{-# OPTIONS_GHC -Wall #-}
module Database.PostgreSQL.PQTypes.Utils where

import Control.Monad

import Database.PostgreSQL.PQTypes.Class
import Database.PostgreSQL.PQTypes.FromRow
import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.Internal.QueryResult
import Database.PostgreSQL.PQTypes.SQL
import Database.PostgreSQL.PQTypes.SQL.Class

runQuery_ :: (IsSQL sql, MonadDB m) => sql -> m ()
runQuery_ = void . runQuery

runQuery01 :: (IsSQL sql, MonadDB m) => sql -> m Bool
runQuery01 sql = do
  n <- runQuery sql
  when (n > 1) $ throwDB AffectedRowsMismatch {
    rowsExpected = [(0, 1)]
  , rowsDelivered = n
  }
  return $ n == 1

runQuery01_ :: (IsSQL sql, MonadDB m) => sql -> m ()
runQuery01_ = void . runQuery01

----------------------------------------

runSQL :: MonadDB m => SQL -> m Int
runSQL = runQuery

runSQL_ :: MonadDB m => SQL -> m ()
runSQL_ = runQuery_

runSQL01 :: MonadDB m => SQL -> m Bool
runSQL01 = runQuery01

runSQL01_ :: MonadDB m => SQL -> m ()
runSQL01_ = runQuery01_

----------------------------------------

fetchMany :: (MonadDB m, FromRow row) => (row -> t) -> m [t]
fetchMany f = foldrM (\row acc -> return $ f row : acc) []

fetchMaybe :: (MonadDB m, FromRow row) => (row -> t) -> m (Maybe t)
fetchMaybe f = flip foldlM Nothing $ \acc row -> case acc of
  Just _ -> do
    Just res <- getQueryResult
    throwDB AffectedRowsMismatch {
      rowsExpected  = [(0, 1)]
    , rowsDelivered = ntuples res
    }
  Nothing -> return . Just . f $ row

fetchOne :: (MonadDB m, FromRow row) => (row -> t) -> m t
fetchOne f = do
  mt <- fetchMaybe f
  case mt of
    Just t  -> return t
    Nothing -> throwDB AffectedRowsMismatch {
      rowsExpected = [(1, 1)]
    , rowsDelivered = 0
    }
