module Database.PostgreSQL.PQTypes.SQL.Observability
  ( SQLDescription(..)
  ) where

import Data.Text (Text)

-- | Structured description for observability.
data SQLDescription = SQLDescription
  { querySummary   :: Text
  , collectionName :: Text
  , operationName  :: Text
  } deriving (Show, Eq)
