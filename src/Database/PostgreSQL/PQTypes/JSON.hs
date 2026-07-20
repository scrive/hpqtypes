module Database.PostgreSQL.PQTypes.JSON
  ( JSON (..)
  , JSONB (..)
  , aesonFromSQL
  , aesonToSQL
  ) where

import Data.Aeson
import Data.Bifunctor
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Text qualified as T
import PostgreSQL.Binary.Decoding qualified as PD
import PostgreSQL.Binary.Encoding qualified as PE

import Database.PostgreSQL.PQTypes.Format
import Database.PostgreSQL.PQTypes.FromSQL
import Database.PostgreSQL.PQTypes.Internal.Oid
import Database.PostgreSQL.PQTypes.ToSQL

-- | Wrapper for (de)serializing underlying type as 'json'.
newtype JSON json = JSON {unJSON :: json}
  deriving stock (Eq, Functor, Ord, Show)

instance PQFormat (JSON json) where
  pqOid = jsonOid
  pqArrayOid = jsonArrayOid

instance FromSQL (JSON BS.ByteString) where
  fromSQL = decodeScalar $ JSON <$> PD.json_bytes Right

instance FromSQL (JSON BSL.ByteString) where
  fromSQL = decodeScalar $ JSON <$> PD.json_bytes (Right . BSL.fromStrict)

instance ToSQL (JSON BS.ByteString) where
  toSQL = Just . PE.json_bytes . unJSON

instance ToSQL (JSON BSL.ByteString) where
  toSQL = Just . PE.json_bytes_lazy . unJSON

instance FromSQL (JSON Value) where
  fromSQL = decodeScalar $ JSON <$> PD.json_ast

instance ToSQL (JSON Value) where
  toSQL = Just . PE.json_ast . unJSON

----------------------------------------

-- | Wrapper for (de)serializing underlying type as 'jsonb'.
newtype JSONB jsonb = JSONB {unJSONB :: jsonb}
  deriving stock (Eq, Functor, Ord, Show)

instance PQFormat (JSONB jsonb) where
  pqOid = jsonbOid
  pqArrayOid = jsonbArrayOid

instance FromSQL (JSONB BS.ByteString) where
  fromSQL = decodeScalar $ JSONB <$> PD.jsonb_bytes Right

instance FromSQL (JSONB BSL.ByteString) where
  fromSQL = decodeScalar $ JSONB <$> PD.jsonb_bytes (Right . BSL.fromStrict)

instance ToSQL (JSONB BS.ByteString) where
  toSQL = Just . PE.jsonb_bytes . unJSONB

instance ToSQL (JSONB BSL.ByteString) where
  toSQL = Just . PE.jsonb_bytes_lazy . unJSONB

instance FromSQL (JSONB Value) where
  fromSQL = decodeScalar $ JSONB <$> PD.jsonb_ast

instance ToSQL (JSONB Value) where
  toSQL = Just . PE.jsonb_ast . unJSONB

----------------------------------------

-- | Helper for defining a 'FromSQL' instance for a type with a 'FromJSON'
-- instance. Inspects 'pqOid' of the type to determine whether it's
-- serialized as 'json' or 'jsonb' (the binary wire formats of the two
-- differ).
--
-- @since 1.9.1.0
aesonFromSQL :: forall a. (PQFormat a, FromJSON a) => RowDecoder a
aesonFromSQL = decodeScalar $ parser decodeValue
  where
    parser =
      if pqOid @a == jsonbOid
        then PD.jsonb_bytes
        else PD.json_bytes
    decodeValue = first T.pack . eitherDecodeStrict'

-- | Helper for defining a 'ToSQL' instance for a type with a 'ToJSON'
-- instance. Inspects 'pqOid' of the type to determine whether it's
-- serialized as 'json' or 'jsonb' (the binary wire formats of the two
-- differ).
--
-- @since 1.9.1.0
aesonToSQL :: forall a. (PQFormat a, ToJSON a) => a -> Maybe PE.Encoding
aesonToSQL = Just . encoder . toJSON
  where
    encoder =
      if pqOid @a == jsonbOid
        then PE.jsonb_ast
        else PE.json_ast
