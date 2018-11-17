module Database.PostgreSQL.PQTypes.SQL.Raw (
    RawSQL
  , rawSQL
  , unRawSQL
  ) where

import Data.String
import Foreign.Marshal.Alloc
import qualified Data.ByteString.Char8 as BS
import qualified Data.Semigroup as SG
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Database.PostgreSQL.PQTypes.SQL.Class
import Database.PostgreSQL.PQTypes.ToRow
import Database.PostgreSQL.PQTypes.ToSQL

-- | Form of SQL query which is very close to libpqtypes specific
-- representation. Note that, in particular, 'RawSQL' () is isomorphic (modulo
-- bottom) to 'Text'.
data RawSQL row = RawSQL !T.Text !row
  deriving (Eq, Ord, Show)

instance (Show row, ToRow row) => IsSQL (RawSQL row) where
  withSQL (RawSQL query row) pa@(ParamAllocator allocParam) execute =
    alloca $ \err -> allocParam $ \param -> do
      toRow row pa param err
      BS.useAsCString (T.encodeUtf8 query) (execute param)

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
