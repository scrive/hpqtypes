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
import Database.PostgreSQL.PQTypes.FromSQL
import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.Internal.QueryResult
import Database.PostgreSQL.PQTypes.Utils

-- | Get current 'QueryResult' or throw an exception if there isn't one.
queryResult
  :: (HasCallStack, MonadDB m, MonadThrow m)
  => m QueryResult
queryResult = withFrozenCallStack $ do
  getQueryResult >>= maybe (throwDB . HPQTypesError $ "queryResult: no query result") pure

----------------------------------------

-- | Fetcher of rows returned by a query as a monadic right fold.
foldrDB
  :: (HasCallStack, MonadDB m)
  => RowDecoder a
  -> (a -> acc -> m acc)
  -> acc
  -> m acc
foldrDB dec f acc = withFrozenCallStack $ do
  getQueryResult >>= maybe (pure acc) (foldrImpl dec f acc)

-- | Fetcher of rows returned by a query as a monadic left fold.
foldlDB
  :: (HasCallStack, MonadDB m)
  => RowDecoder a
  -> (acc -> a -> m acc)
  -> acc
  -> m acc
foldlDB dec f acc = withFrozenCallStack $ do
  getQueryResult >>= maybe (pure acc) (foldlImpl dec f acc)

-- | Fetcher of rows returned by a query as a monadic map.
mapDB_
  :: (HasCallStack, MonadDB m)
  => RowDecoder a
  -> (a -> m r)
  -> m ()
mapDB_ dec f = withFrozenCallStack $ do
  getQueryResult >>= maybe (pure ()) (foldlImpl dec (\() a -> void (f a)) ())

----------------------------------------

-- | Specialization of 'foldrDB' that fetches a list of rows.
fetchMany :: (HasCallStack, MonadDB m) => RowDecoder a -> m [a]
fetchMany dec = withFrozenCallStack $ foldrDB dec (\a acc -> pure $ a : acc) []

-- | Specialization of 'foldlDB' that fetches one or zero rows. If
-- more rows are delivered, 'AffectedRowsMismatch' exception is thrown.
fetchMaybe
  :: (HasCallStack, MonadDB m, MonadThrow m)
  => RowDecoder a
  -> m (Maybe a)
fetchMaybe dec = withFrozenCallStack $ do
  getQueryResult >>= \case
    Nothing -> pure Nothing
    Just qr -> foldlImpl dec (go qr) Nothing qr
  where
    go _ Nothing a = pure $ Just a
    go qr (Just _) _ =
      throwDB
        AffectedRowsMismatch
          { rowsExpected = [(0, 1)]
          , rowsDelivered = ntuples qr
          }

-- | Specialization of 'fetchMaybe' that fetches exactly one row. If
-- no row is delivered, 'AffectedRowsMismatch' exception is thrown.
fetchOne :: (HasCallStack, MonadDB m, MonadThrow m) => RowDecoder a -> m a
fetchOne dec = withFrozenCallStack $ do
  ma <- fetchMaybe dec
  case ma of
    Just a -> pure a
    Nothing ->
      throwDB
        AffectedRowsMismatch
          { rowsExpected = [(1, 1)]
          , rowsDelivered = 0
          }
