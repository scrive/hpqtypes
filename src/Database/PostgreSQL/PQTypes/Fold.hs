module Database.PostgreSQL.PQTypes.Fold (
    queryResult
  , foldrDB
  , foldlDB
  , mapDB_
  , fetchMany
  , fetchMaybe
  , fetchOne
  ) where

import Control.Applicative
import Control.Monad.Catch
import Prelude
import qualified Data.Foldable as F

import Database.PostgreSQL.PQTypes.Class
import Database.PostgreSQL.PQTypes.FromRow
import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.Internal.QueryResult
import Database.PostgreSQL.PQTypes.Utils

-- | Get current 'QueryResult' or throw an exception if there isn't one.
{-# INLINABLE queryResult #-}
queryResult :: (MonadDB m, MonadThrow m, FromRow row) => m (QueryResult row)
queryResult = getQueryResult
  >>= maybe (throwDB . HPQTypesError $ "queryResult: no query result") return

----------------------------------------

-- | Specialization of 'F.foldrM' for convenient query results fetching.
{-# INLINABLE foldrDB #-}
foldrDB :: (MonadDB m, FromRow row) => (row -> acc -> m acc) -> acc -> m acc
foldrDB f acc = maybe (return acc) (F.foldrM f acc) =<< getQueryResult

-- | Specialization of 'F.foldlM' for convenient query results fetching.
{-# INLINABLE foldlDB #-}
foldlDB :: (MonadDB m, FromRow row) => (acc -> row -> m acc) -> acc -> m acc
foldlDB f acc = maybe (return acc) (F.foldlM f acc) =<< getQueryResult

-- | Specialization of 'F.mapM_' for convenient mapping over query results.
{-# INLINABLE mapDB_ #-}
mapDB_ :: (MonadDB m, FromRow row) => (row -> m t) -> m ()
mapDB_ f = maybe (return ()) (F.mapM_ f) =<< getQueryResult

----------------------------------------

-- | Specialization of 'foldrDB' that fetches a list of rows.
{-# INLINABLE fetchMany #-}
fetchMany :: (MonadDB m, FromRow row) => (row -> t) -> m [t]
fetchMany f = foldrDB (\row acc -> return $ f row : acc) []

-- | Specialization of 'foldlDB' that fetches one or zero rows. If
-- more rows are delivered, 'AffectedRowsMismatch' exception is thrown.
{-# INLINABLE fetchMaybe #-}
fetchMaybe :: (MonadDB m, MonadThrow m, FromRow row) => (row -> t) -> m (Maybe t)
fetchMaybe f = getQueryResult >>= \mqr -> case mqr of
  Nothing -> return Nothing
  Just qr -> fst <$> foldlDB go (Nothing, f <$> qr)
  where
    go (Nothing, qr) row = return (Just $ f row, qr)
    go (Just _, qr) _ = throwDB AffectedRowsMismatch {
        rowsExpected  = [(0, 1)]
      , rowsDelivered = ntuples qr
      }

-- | Specialization of 'fetchMaybe' that fetches exactly one row. If
-- no row is delivered, 'AffectedRowsMismatch' exception is thrown.
{-# INLINABLE fetchOne #-}
fetchOne :: (MonadDB m, MonadThrow m, FromRow row) => (row -> t) -> m t
fetchOne f = do
  mt <- fetchMaybe f
  case mt of
    Just t  -> return t
    Nothing -> throwDB AffectedRowsMismatch {
      rowsExpected = [(1, 1)]
    , rowsDelivered = 0
    }
