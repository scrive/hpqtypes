module Database.PostgreSQL.PQTypes.Model.EnumType
  ( EnumType (..)
  , sqlCreateEnum
  , sqlDropEnum
  , sqlAddEnumValue
  ) where

import Data.Monoid.Utils
import Data.Text qualified as T
import Database.PostgreSQL.PQTypes

data EnumType = EnumType
  { etName :: !(RawSQL ())
  , etValues :: ![RawSQL ()]
  }
  deriving (Eq, Ord, Show)

-- | Make SQL query that creates an enum type.
sqlCreateEnum :: EnumType -> RawSQL ()
sqlCreateEnum EnumType {..} =
  smconcat
    [ "CREATE TYPE"
    , etName
    , "AS ENUM ("
    , mintercalate ", " $ map quotedValue etValues
    , ")"
    ]
  where
    quotedValue v = rawSQL ("'" <> T.replace "'" "''" (unRawSQL v) <> "'" :: T.Text) ()

-- | Make SQL query that drops a composite type.
sqlDropEnum :: RawSQL () -> RawSQL ()
sqlDropEnum = ("DROP TYPE" <+>)

-- | Add a value to an enum type
sqlAddEnumValue :: RawSQL () -> RawSQL () -> RawSQL ()
sqlAddEnumValue enumName value = "ALTER TYPE " <> enumName <> " ADD VALUE '" <> value <> "'"
