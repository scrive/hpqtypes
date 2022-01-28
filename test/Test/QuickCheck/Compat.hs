{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
module Test.QuickCheck.Compat (
    QCGen
  , newQCGen
  , mkQCGen
  , Test.QuickCheck.Compat.fromList
  , Value0
  , mkValue0
  ) where

import Data.Aeson

#if MIN_VERSION_QuickCheck(2,7,0)

import Test.QuickCheck.Random

#else

import System.Random

type QCGen = StdGen

newQCGen :: IO QCGen
newQCGen = newStdGen

mkQCGen :: Int -> QCGen
mkQCGen = mkStdGen

#endif

#if MIN_VERSION_aeson(2,0,1)
import Data.Aeson.KeyMap as Map

import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes

fromList :: [(Key, v)] -> Map.KeyMap v
fromList = Map.fromList

newtype Value0 = Value0 { unValue0 :: Value }
  deriving newtype (Eq, Show)

instance ToSQL (JSONB Value0) where
  type PQDest (JSONB Value0) = PGbytea
  toSQL = aesonToSQL . unValue0 . unJSONB

instance FromSQL (JSONB Value0) where
  type PQBase (JSONB Value0) = PGbytea
  fromSQL = fmap (JSONB . Value0) . aesonFromSQL

mkValue0 :: Value -> Value0
mkValue0 = Value0
#else
import Data.HashMap.Strict as Map
import Data.Hashable

fromList :: (Eq k, Hashable k) => [(k, v)] -> Map.HashMap k v
fromList = Map.fromList

type Value0 = Value

mkValue0 :: Value0 -> Value0
mkValue0 = id
#endif

