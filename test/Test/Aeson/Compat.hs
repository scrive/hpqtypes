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
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM

fromList :: [(Text, v)] -> KM.KeyMap v
fromList = KM.fromList . map (first K.fromText)

newtype Value0 = Value0 Value
  deriving newtype (Eq, FromJSON, Show, ToJSON)

mkValue0 :: Value -> Value0
mkValue0 = Value0

#else

import Data.HashMap.Strict qualified as HM

fromList :: [(Text, v)] -> HM.HashMap Text v
fromList = HM.fromList

type Value0 = Value

mkValue0 :: Value0 -> Value0
mkValue0 = id

#endif
