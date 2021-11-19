module Database.PostgreSQL.PQTypes.Exception
  ( DBException(..)
  , rethrowWithContext
  , throwDB
  ) where

import Control.Exception
import Control.Monad.Catch

import Database.PostgreSQL.PQTypes.Class
import Database.PostgreSQL.PQTypes.Internal.Exception
import Database.PostgreSQL.PQTypes.SQL.Class

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
