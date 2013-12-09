{-# LANGUAGE FlexibleInstances #-}
module Database.PostgreSQL.PQTypes.SQL.Raw (
    RawSQL
  , rawSQL
  ) where

import Data.Monoid
import Data.String
import Foreign.Marshal.Alloc
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Monoid.Space
import Database.PostgreSQL.PQTypes.Format
import Database.PostgreSQL.PQTypes.SQL.Class
import Database.PostgreSQL.PQTypes.ToRow

-- | Form of SQL query which is very close to libpqtypes specific
-- representation. Note that, in particular, 'RawSQL' () is
-- isomorphic (modulo bottom) to 'ByteString'.
data RawSQL row = RawSQL !BS.ByteString !row
  deriving (Eq, Ord, Show)

instance (Show row, ToRow row) => IsSQL (RawSQL row) where
  someSQL = SomeSQL
  withSQL (RawSQL query row) allocParam execute = alloca $ \err ->
    allocParam $ \param -> BS.useAsCString (pqFormat row) $ \fmt -> do
      toRow row allocParam param err fmt
      BS.useAsCString query (execute param)

-- | Construct 'RawSQL' () from 'String'. The underlying 'ByteString'
-- will be encoded as UTF-8, so if you are working with a different
-- encoding, you should not rely on this instance.
instance IsString (RawSQL ()) where
  fromString = flip RawSQL () . T.encodeUtf8 . T.pack

instance Monoid (RawSQL ()) where
  mempty = rawSQL BS.empty ()
  RawSQL a () `mappend` RawSQL b () = RawSQL (a `mappend` b) ()
  mconcat xs = RawSQL (BS.concat $ map (\(RawSQL s ()) -> s) xs) ()

instance SpaceMonoid (RawSQL ()) where
  mspace = RawSQL mspace ()

-- | Construct 'RawSQL' from 'ByteString' and a tuple of parameters.
rawSQL :: (Show row, ToRow row) => BS.ByteString -> row -> RawSQL row
rawSQL = RawSQL
