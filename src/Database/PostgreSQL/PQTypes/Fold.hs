module Database.PostgreSQL.PQTypes.Fold
  ( queryResult
  , foldrDB
  , foldlDB
  , mapDB_
  , fetchMany
  , fetchMaybe
  , fetchOne
  ) where

import Control.Monad.Catch
import Data.Functor
import GHC.Stack

import Database.PostgreSQL.PQTypes.Class
import Database.PostgreSQL.PQTypes.FromRow
import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.Internal.QueryResult
import Database.PostgreSQL.PQTypes.Utils

-- | Get current 'QueryResult' or throw an exception if there isn't one.
queryResult
  :: (HasCallStack, MonadDB m, MonadThrow m, FromRow row)
  => m (QueryResult row)
queryResult =
  withFrozenCallStack $
    getQueryResult
      >>= maybe (throwDB . HPQTypesError $ "queryResult: no query result") pure

----------------------------------------

-- | Fetcher of rows returned by a query as a monadic right fold.
foldrDB
  :: (HasCallStack, MonadDB m, FromRow row)
  => (row -> acc -> m acc)
  -> acc
  -> m acc
foldrDB f acc =
  withFrozenCallStack $
    getQueryResult
      >>= maybe (pure acc) (foldrImpl False f acc)

-- | Fetcher of rows returned by a query as a monadic left fold.
foldlDB
  :: (HasCallStack, MonadDB m, FromRow row)
  => (acc -> row -> m acc)
  -> acc
  -> m acc
foldlDB f acc =
  withFrozenCallStack $
    getQueryResult
      >>= maybe (pure acc) (foldlImpl False f acc)

-- | Fetcher of rows returned by a query as a monadic map.
mapDB_
  :: (HasCallStack, MonadDB m, FromRow row)
  => (row -> m r)
  -> m ()
mapDB_ f =
  withFrozenCallStack $
    getQueryResult
      >>= maybe (pure ()) (foldlImpl False (\() row -> void (f row)) ())

----------------------------------------

-- | Specialization of 'foldrDB' that fetches a list of rows.
fetchMany :: (HasCallStack, MonadDB m, FromRow row) => (row -> t) -> m [t]
fetchMany f = withFrozenCallStack $ foldrDB (\row acc -> pure $ f row : acc) []

-- | Specialization of 'foldlDB' that fetches one or zero rows. If
-- more rows are delivered, 'AffectedRowsMismatch' exception is thrown.
fetchMaybe
  :: (HasCallStack, MonadDB m, MonadThrow m, FromRow row)
  => (row -> t)
  -> m (Maybe t)
fetchMaybe f = withFrozenCallStack $ do
  getQueryResult >>= \case
    Nothing -> pure Nothing
    Just qr -> fst <$> foldlDB go (Nothing, f <$> qr)
  where
    go (Nothing, qr) row = pure (Just $ f row, qr)
    go (Just _, qr) _ =
      throwDB
        AffectedRowsMismatch
          { rowsExpected = [(0, 1)]
          , rowsDelivered = ntuples qr
          }

-- | Specialization of 'fetchMaybe' that fetches exactly one row. If
-- no row is delivered, 'AffectedRowsMismatch' exception is thrown.
fetchOne :: (HasCallStack, MonadDB m, MonadThrow m, FromRow row) => (row -> t) -> m t
fetchOne f = withFrozenCallStack $ do
  mt <- fetchMaybe f
  case mt of
    Just t -> pure t
    Nothing ->
      throwDB
        AffectedRowsMismatch
          { rowsExpected = [(1, 1)]
          , rowsDelivered = 0
          }
