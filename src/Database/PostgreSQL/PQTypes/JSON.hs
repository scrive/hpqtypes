module Database.PostgreSQL.PQTypes.JSON (
    JSON(..)
  , JSONB(..)
  ) where

import Control.Applicative
import Data.Aeson
import Data.Typeable
import Foreign.Ptr
import Prelude
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

import Database.PostgreSQL.PQTypes.Format
import Database.PostgreSQL.PQTypes.FromSQL
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.ToSQL

-- | Wrapper for (de)serializing underlying type as 'json'.
newtype JSON json = JSON { unJSON :: json }
  deriving (Eq, Functor, Ord, Show, Typeable)

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
  fromSQL = valueFromSQL JSON

instance ToSQL (JSON Value) where
  type PQDest (JSON Value) = PGbytea
  toSQL = valueToSQL unJSON

----------------------------------------

-- | Wrapper for (de)serializing underlying type as 'jsonb'.
newtype JSONB jsonb = JSONB { unJSONB :: jsonb }
  deriving (Eq, Functor, Ord, Show, Typeable)

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
  fromSQL = valueFromSQL JSONB

instance ToSQL (JSONB Value) where
  type PQDest (JSONB Value) = PGbytea
  toSQL = valueToSQL unJSONB

----------------------------------------

valueFromSQL :: (Value -> json) -> Maybe PGbytea -> IO json
valueFromSQL jsonCon mbase = do
  evalue <- eitherDecodeStrict' <$> fromSQL mbase
  case evalue of
    Left err -> E.throwIO . E.ErrorCall $ "valueFromSQL: " ++ err
    Right value -> return $ jsonCon value

valueToSQL :: (json -> Value)
           -> json
           -> ParamAllocator
           -> (Ptr PGbytea -> IO r)
           -> IO r
valueToSQL jsonDecon = toSQL . BSL.toStrict . encode . jsonDecon
