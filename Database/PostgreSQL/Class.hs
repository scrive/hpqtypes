{-# OPTIONS_GHC -Wall #-}
module Database.PostgreSQL.Class where

import Database.PostgreSQL.Internal.C.Types
import Database.PostgreSQL.Internal.SQL
import Database.PostgreSQL.Row

class Monad m => MonadDB m where
  runQuery  :: SQL -> m ()
  queryResult :: m (Maybe QueryResult)
  fold :: Row base dest => (acc -> dest -> m acc) -> acc -> m acc
