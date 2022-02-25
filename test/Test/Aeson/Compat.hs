{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
module Test.Aeson.Compat
  ( fromList
  , Value0
  , mkValue0
  ) where

import Data.Aeson
import Data.Text (Text)

#if MIN_VERSION_aeson(2,0,0)

import Data.Bifunctor (first)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM

import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes

fromList :: [(Text, v)] -> KM.KeyMap v
fromList = KM.fromList . map (first K.fromText)

newtype Value0 = Value0 { unValue0 :: Value }
  deriving newtype (Eq, Show)

instance ToSQL (JSON Value0) where
  type PQDest (JSON Value0) = PGbytea
  toSQL = aesonToSQL . unValue0 . unJSON

instance FromSQL (JSON Value0) where
  type PQBase (JSON Value0) = PGbytea
  fromSQL = fmap (JSON . Value0) . aesonFromSQL

instance ToSQL (JSONB Value0) where
  type PQDest (JSONB Value0) = PGbytea
  toSQL = aesonToSQL . unValue0 . unJSONB

instance FromSQL (JSONB Value0) where
  type PQBase (JSONB Value0) = PGbytea
  fromSQL = fmap (JSONB . Value0) . aesonFromSQL

mkValue0 :: Value -> Value0
mkValue0 = Value0

#else

import qualified Data.HashMap.Strict as HM

fromList :: [(Text, v)] -> HM.HashMap Text v
fromList = HM.fromList

type Value0 = Value

mkValue0 :: Value0 -> Value0
mkValue0 = id

#endif
