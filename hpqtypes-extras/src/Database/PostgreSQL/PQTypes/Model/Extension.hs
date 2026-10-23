module Database.PostgreSQL.PQTypes.Model.Extension
  ( Extension (..)
  , ununExtension
  ) where

import Data.String
import Data.Text (Text)
import Database.PostgreSQL.PQTypes

newtype Extension = Extension {unExtension :: RawSQL ()}
  deriving (Eq, Ord, Show, IsString)

ununExtension :: Extension -> Text
ununExtension = unRawSQL . unExtension
