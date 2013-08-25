module DB.Class where

import Data.ByteString (ByteString)
import Foreign.Storable

import DB.Row
import DB.Primitive.Types

class Monad m => MonadDB m where
  runQuery  :: String -> m ()
  queryResult :: m (Maybe QueryResult)
  fold :: Row base dest => (acc -> dest -> m acc) -> acc -> m acc
