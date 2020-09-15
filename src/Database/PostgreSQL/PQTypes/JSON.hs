{-# LANGUAGE TypeApplications #-}
module Database.PostgreSQL.PQTypes.JSON
  ( JSON(..)
  , JSONB(..)
  , aesonFromSQL
  , aesonToSQL
  ) where

import Data.Aeson
import Foreign.Ptr
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

import Database.PostgreSQL.PQTypes.Format
import Database.PostgreSQL.PQTypes.FromSQL
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.ToSQL

-- | Wrapper for (de)serializing underlying type as 'json'.
newtype JSON json = JSON { unJSON :: json }
  deriving (Eq, Functor, Ord, Show)

instance PQFormat (JSON json) where
  pqFormat = BS.pack "%json"

instance FromSQL (JSON BS.ByteString) where
  type PQBase (JSON BS.ByteString) = PGbytea
  fromSQL = fmap JSON . fromSQL

instance FromSQL (JSON BSL.ByteString) where
  type PQBase (JSON BSL.ByteString) = PGbytea
  fromSQL = fmap JSON . fromSQL

instance ToSQL (JSON BS.ByteString) where
  type PQDest (JSON BS.ByteString) = PGbytea
  toSQL = toSQL . unJSON

instance ToSQL (JSON BSL.ByteString) where
  type PQDest (JSON BSL.ByteString) = PGbytea
  toSQL = toSQL . unJSON

instance FromSQL (JSON Value) where
  type PQBase (JSON Value) = PGbytea
  fromSQL = fmap JSON . aesonFromSQL

instance ToSQL (JSON Value) where
  type PQDest (JSON Value) = PGbytea
  toSQL = aesonToSQL . unJSON

----------------------------------------

-- | Wrapper for (de)serializing underlying type as 'jsonb'.
newtype JSONB jsonb = JSONB { unJSONB :: jsonb }
  deriving (Eq, Functor, Ord, Show)

instance PQFormat (JSONB jsonb) where
  pqFormat = BS.pack "%jsonb"

instance FromSQL (JSONB BS.ByteString) where
  type PQBase (JSONB BS.ByteString) = PGbytea
  fromSQL = fmap JSONB . fromSQL

instance FromSQL (JSONB BSL.ByteString) where
  type PQBase (JSONB BSL.ByteString) = PGbytea
  fromSQL = fmap JSONB . fromSQL

instance ToSQL (JSONB BS.ByteString) where
  type PQDest (JSONB BS.ByteString) = PGbytea
  toSQL = toSQL . unJSONB

instance ToSQL (JSONB BSL.ByteString) where
  type PQDest (JSONB BSL.ByteString) = PGbytea
  toSQL = toSQL . unJSONB

instance FromSQL (JSONB Value) where
  type PQBase (JSONB Value) = PGbytea
  fromSQL = fmap JSONB . aesonFromSQL

instance ToSQL (JSONB Value) where
  type PQDest (JSONB Value) = PGbytea
  toSQL = aesonToSQL . unJSONB

----------------------------------------

-- | Helper for defining 'FromSQL' instance for a type with 'FromJSON' instance.
--
-- @since 1.9.1.0
aesonFromSQL :: FromJSON t => Maybe PGbytea -> IO t
aesonFromSQL mbase = do
  evalue <- eitherDecodeStrict' <$> fromSQL mbase
  case evalue of
    Left err -> E.throwIO . E.ErrorCall $ "aesonFromSQL: " ++ err
    Right value -> return value

-- | Helper for defining 'ToSQL' instance for a type with 'ToJSON' instance.
--
-- @since 1.9.1.0
aesonToSQL
  :: ToJSON t
  => t
  -> ParamAllocator
  -> (Ptr PGbytea -> IO r)
  -> IO r
aesonToSQL = toSQL . BSL.toStrict . encode
