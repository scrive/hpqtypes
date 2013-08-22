module DB.Class where

import Data.ByteString (ByteString)
import Foreign.Storable

import DB.FromSQL

class MonadDB m where
  run :: ByteString -> m ()
  fold :: FromSQL base dest => (acc -> dest -> acc) -> acc -> m acc
