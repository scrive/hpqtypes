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

import Database.PostgreSQL.PQTypes.Format
import Database.PostgreSQL.PQTypes.FromSQL
import Database.PostgreSQL.PQTypes.Internal.Decoding qualified as PD
import Database.PostgreSQL.PQTypes.Internal.Encoding qualified as PE
import Database.PostgreSQL.PQTypes.Internal.Oid
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
  deriving stock (Eq, Functor, Ord, Show)

instance PQFormat (JSON a) where
  pqOid = jsonOid
  pqArrayOid = jsonArrayOid

instance (FromJSON a, Typeable a) => FromSQL (JSON a) where
  fromSQL = decodeScalar $ JSON <$> PD.json_aeson

instance ToJSON a => ToSQL (JSON a) where
  toSQL = Just . PE.json_aeson . unJSON

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
  deriving stock (Eq, Functor, Ord, Show)

instance PQFormat (JSONB a) where
  pqOid = jsonbOid
  pqArrayOid = jsonbArrayOid

instance (FromJSON a, Typeable a) => FromSQL (JSONB a) where
  fromSQL = decodeScalar $ JSONB <$> PD.jsonb_aeson

instance ToJSON a => ToSQL (JSONB a) where
  toSQL = Just . PE.jsonb_aeson . unJSONB

----------------------------------------

-- | A @json@ value as its unparsed UTF-8 text.
newtype RawJSON = RawJSON {unRawJSON :: BS.ByteString}
  deriving stock (Eq, Ord, Show)

instance PQFormat RawJSON where
  pqOid = jsonOid
  pqArrayOid = jsonArrayOid

instance FromSQL RawJSON where
  fromSQL = decodeScalar $ RawJSON <$> PD.json_bytes Right

instance ToSQL RawJSON where
  toSQL = Just . PE.json_bytes . unRawJSON

-- | Encode a value with its 'ToJSON' instance.
encodeRawJSON :: ToJSON a => a -> RawJSON
encodeRawJSON = RawJSON . BSL.toStrict . encode

-- | Decode a value with its 'FromJSON' instance.
decodeRawJSON :: FromJSON a => RawJSON -> Maybe a
decodeRawJSON = decodeStrict' . unRawJSON

-- | Decode a value with its 'FromJSON' instance and report the reason for a
-- failure.
eitherDecodeRawJSON :: FromJSON a => RawJSON -> Either String a
eitherDecodeRawJSON = eitherDecodeStrict' . unRawJSON

----------------------------------------

-- | A @jsonb@ value as its unparsed UTF-8 text.
newtype RawJSONB = RawJSONB {unRawJSONB :: BS.ByteString}
  deriving stock (Eq, Ord, Show)

instance PQFormat RawJSONB where
  pqOid = jsonbOid
  pqArrayOid = jsonbArrayOid

instance FromSQL RawJSONB where
  fromSQL = decodeScalar $ RawJSONB <$> PD.jsonb_bytes Right

instance ToSQL RawJSONB where
  toSQL = Just . PE.jsonb_bytes . unRawJSONB

-- | Encode a value with its 'ToJSON' instance.
--
-- /Note:/ The server normalizes a @jsonb@ value on input, e.g. it reorders the
-- keys of an object and drops insignificant whitespace. The result of
-- 'encodeRawJSONB' therefore differs in general from the text that the server
-- returns for the same value.
encodeRawJSONB :: ToJSON a => a -> RawJSONB
encodeRawJSONB = RawJSONB . BSL.toStrict . encode

-- | Decode a value with its 'FromJSON' instance.
decodeRawJSONB :: FromJSON a => RawJSONB -> Maybe a
decodeRawJSONB = decodeStrict' . unRawJSONB

-- | Decode a value with its 'FromJSON' instance and report the reason for a
-- failure.
eitherDecodeRawJSONB :: FromJSON a => RawJSONB -> Either String a
eitherDecodeRawJSONB = eitherDecodeStrict' . unRawJSONB
