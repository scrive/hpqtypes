module Database.PostgreSQL.PQTypes.JSON
  ( -- * Helpers, to be used with @deriving via@ (@-XDerivingVia@).
    JSON (..)
  , JSONB (..)

    -- * Unparsed JSON values
  , RawJSON (..)
  , encodeRawJSON
  , decodeRawJSON
  , eitherDecodeRawJSON
  , RawJSONB (..)
  , encodeRawJSONB
  , decodeRawJSONB
  , eitherDecodeRawJSONB
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

-- | Encode a value with its 'ToJSON' instance.
--
-- @since 1.15.0.0
encodeRawJSON :: ToJSON a => a -> RawJSON
encodeRawJSON = RawJSON . BSL.toStrict . encode

-- | Decode a value with its 'FromJSON' instance.
--
-- @since 1.15.0.0
decodeRawJSON :: FromJSON a => RawJSON -> Maybe a
decodeRawJSON = decodeStrict' . unRawJSON

-- | Decode a value with its 'FromJSON' instance and report the reason for a
-- failure.
--
-- @since 1.15.0.0
eitherDecodeRawJSON :: FromJSON a => RawJSON -> Either String a
eitherDecodeRawJSON = eitherDecodeStrict' . unRawJSON

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

-- | Encode a value with its 'ToJSON' instance.
--
-- /Note:/ The server normalizes a @jsonb@ value on input, e.g. it reorders the
-- keys of an object and drops insignificant whitespace. The result of
-- 'encodeRawJSONB' therefore differs in general from the text that the server
-- returns for the same value.
--
-- @since 1.15.0.0
encodeRawJSONB :: ToJSON a => a -> RawJSONB
encodeRawJSONB = RawJSONB . BSL.toStrict . encode

-- | Decode a value with its 'FromJSON' instance.
--
-- @since 1.15.0.0
decodeRawJSONB :: FromJSON a => RawJSONB -> Maybe a
decodeRawJSONB = decodeStrict' . unRawJSONB

-- | Decode a value with its 'FromJSON' instance and report the reason for a
-- failure.
--
-- @since 1.15.0.0
eitherDecodeRawJSONB :: FromJSON a => RawJSONB -> Either String a
eitherDecodeRawJSONB = eitherDecodeStrict' . unRawJSONB

----------------------------------------

aesonFromSQL :: forall a. (FromJSON a, Typeable a) => Maybe PGbytea -> IO a
aesonFromSQL mbase = do
  v <- fromSQL mbase
  case eitherDecodeStrict' v of
    Right a -> pure a
    Left err -> hpqTypesError $ "aesonFromSQL (" ++ show (typeRep $ Proxy @a) ++ "): " ++ err

aesonToSQL :: ToJSON a => a -> ParamAllocator -> (Ptr PGbytea -> IO r) -> IO r
aesonToSQL = toSQL . BSL.toStrict . encode
