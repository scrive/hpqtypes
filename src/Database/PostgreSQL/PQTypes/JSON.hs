module Database.PostgreSQL.PQTypes.JSON
  ( -- * Helpers, to be used with @deriving via@ (@-XDerivingVia@).
    JSON (..)
  , JSONB (..)

    -- * Unparsed JSON values
  , RawJSON (..)
  , RawJSONB (..)
  ) where

import Data.Aeson
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Typeable
import Foreign.Ptr

import Database.PostgreSQL.PQTypes.Format
import Database.PostgreSQL.PQTypes.FromSQL
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Utils
import Database.PostgreSQL.PQTypes.ToSQL

-- | Wrapper that serializes and deserializes the underlying type as @json@
-- with its 'ToJSON' and 'FromJSON' instances.
--
-- /Note:/ To get the SQL instances for a type of your own, use
-- @DerivingVia@:
--
-- @
-- data Foo = ...
--   deriving anyclass (FromJSON, ToJSON)
--   deriving (PQFormat, ToSQL, FromSQL) via JSON Foo
-- @
newtype JSON a = JSON {unJSON :: a}
  deriving (Eq, Functor, Ord, Show)

instance PQFormat (JSON a) where
  pqFormat = BS.pack "%json"

instance (FromJSON a, Typeable a) => FromSQL (JSON a) where
  type PQBase (JSON a) = PGbytea
  fromSQL = fmap JSON . aesonFromSQL

instance ToJSON a => ToSQL (JSON a) where
  type PQDest (JSON a) = PGbytea
  toSQL = aesonToSQL . unJSON

----------------------------------------

-- | Wrapper that serializes and deserializes the underlying type as @jsonb@
-- with its 'ToJSON' and 'FromJSON' instances.
--
-- /Note:/ To get the SQL instances for a type of your own, use
-- @DerivingVia@:
--
-- @
-- data Foo = ...
--   deriving anyclass (FromJSON, ToJSON)
--   deriving (PQFormat, ToSQL, FromSQL) via JSONB Foo
-- @
newtype JSONB a = JSONB {unJSONB :: a}
  deriving (Eq, Functor, Ord, Show)

instance PQFormat (JSONB a) where
  pqFormat = BS.pack "%jsonb"

instance (FromJSON a, Typeable a) => FromSQL (JSONB a) where
  type PQBase (JSONB a) = PGbytea
  fromSQL = fmap JSONB . aesonFromSQL

instance ToJSON a => ToSQL (JSONB a) where
  type PQDest (JSONB a) = PGbytea
  toSQL = aesonToSQL . unJSONB

----------------------------------------

-- | A @json@ value as its unparsed UTF-8 text.
newtype RawJSON = RawJSON {unRawJSON :: BS.ByteString}
  deriving (Eq, Ord, Show)

instance PQFormat RawJSON where
  pqFormat = BS.pack "%json"

instance FromSQL RawJSON where
  type PQBase RawJSON = PGbytea
  fromSQL = fmap RawJSON . fromSQL

instance ToSQL RawJSON where
  type PQDest RawJSON = PGbytea
  toSQL = toSQL . unRawJSON

----------------------------------------

-- | A @jsonb@ value as its unparsed UTF-8 text.
newtype RawJSONB = RawJSONB {unRawJSONB :: BS.ByteString}
  deriving (Eq, Ord, Show)

instance PQFormat RawJSONB where
  pqFormat = BS.pack "%jsonb"

instance FromSQL RawJSONB where
  type PQBase RawJSONB = PGbytea
  fromSQL = fmap RawJSONB . fromSQL

instance ToSQL RawJSONB where
  type PQDest RawJSONB = PGbytea
  toSQL = toSQL . unRawJSONB

----------------------------------------

aesonFromSQL :: forall a. (FromJSON a, Typeable a) => Maybe PGbytea -> IO a
aesonFromSQL mbase = do
  v <- fromSQL mbase
  case eitherDecodeStrict' v of
    Right a -> pure a
    Left err -> hpqTypesError $ "aesonFromSQL (" ++ show (typeRep $ Proxy @a) ++ "): " ++ err

aesonToSQL :: ToJSON a => a -> ParamAllocator -> (Ptr PGbytea -> IO r) -> IO r
aesonToSQL = toSQL . BSL.toStrict . encode
