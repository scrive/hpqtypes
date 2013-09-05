{-# OPTIONS_GHC -Wall #-}
module Database.PostgreSQL.Class where

import Database.PostgreSQL.Internal.State
import Database.PostgreSQL.Internal.SQL
import Database.PostgreSQL.Row

class Monad m => MonadDB m where
  runQuery     :: SQL -> m Int
  getLastQuery :: m SQL

  getQueryResult   :: m (Maybe QueryResult)
  clearQueryResult :: m ()

  getTransactionMode :: m TransactionMode
  setTransactionMode :: TransactionMode -> m ()

  fold :: Row base dest => (acc -> dest -> m acc) -> acc -> m acc
