{-# OPTIONS_GHC -Wall #-}
module DB.Class where

import DB.Primitive.Types
import DB.Row
import DB.SQL

class Monad m => MonadDB m where
  runQuery  :: SQL -> m ()
  queryResult :: m (Maybe QueryResult)
  fold :: Row base dest => (acc -> dest -> m acc) -> acc -> m acc
