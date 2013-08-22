module DB.Class where

import Data.ByteString (ByteString)
import Foreign.Storable

import DB.Row

class Monad m => MonadDB m where
  run  :: ByteString -> m ()
  fold :: Row base dest => (acc -> dest -> IO acc) -> acc -> m acc
