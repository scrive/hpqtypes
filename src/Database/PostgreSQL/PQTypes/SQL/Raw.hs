module Database.PostgreSQL.PQTypes.SQL.Raw
  ( RawSQL
  , rawSQL
  , unRawSQL
  ) where

import Data.ByteString.Char8 qualified as BS
import Data.Semigroup qualified as SG
import Data.String
import Data.Text qualified as T
import Data.Text.Encoding qualified as T

import Database.PostgreSQL.PQTypes.SQL.Class
import Database.PostgreSQL.PQTypes.ToRow

-- | Form of SQL query which is very close to the libpq representation, i.e.
-- the query with placeholders @$1@, @$2@, ... along with a tuple of their
-- values. Note that, in particular, 'RawSQL' () is isomorphic (modulo bottom)
-- to 'Text'.
data RawSQL row = RawSQL !T.Text !row
  deriving stock (Eq, Ord, Show)

instance (Show row, ToRow row) => IsSQL (RawSQL row) where
  withSQL (RawSQL query row) execute =
    BS.useAsCString (T.encodeUtf8 query) $ \cquery ->
      execute cquery (toRow row)

-- | Construct 'RawSQL' () from 'String'.
instance IsString (RawSQL ()) where
  fromString = flip RawSQL () . T.pack

instance SG.Semigroup (RawSQL ()) where
  RawSQL a () <> RawSQL b () = RawSQL (a SG.<> b) ()
  sconcat xs = RawSQL (SG.sconcat $ fmap (\(RawSQL s ()) -> s) xs) ()

instance Monoid (RawSQL ()) where
  mempty = rawSQL T.empty ()
  mappend = (SG.<>)
  mconcat xs = RawSQL (mconcat $ fmap (\(RawSQL s ()) -> s) xs) ()

-- | Construct 'RawSQL' from 'Text' and a tuple of parameters.
rawSQL :: (Show row, ToRow row) => T.Text -> row -> RawSQL row
rawSQL = RawSQL

-- | Take query string out of 'RawSQL' ().
unRawSQL :: RawSQL () -> T.Text
unRawSQL (RawSQL s _) = s
