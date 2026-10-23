module Database.PostgreSQL.PQTypes.Model.PrimaryKey
  ( PrimaryKey
  , pkOnColumn
  , pkOnColumns
  , pkName
  , pkColumns
  , sqlAddPK
  , sqlAddPKUsing
  , sqlDropPK
  ) where

import Data.Monoid.Utils
import Database.PostgreSQL.PQTypes
import Database.PostgreSQL.PQTypes.Model.Index
import Database.PostgreSQL.PQTypes.Utils.NubList

newtype PrimaryKey = PrimaryKey (NubList (RawSQL ()))
  deriving (Eq, Show)

pkOnColumn :: RawSQL () -> Maybe PrimaryKey
pkOnColumn column = Just . PrimaryKey . toNubList $ [column]

pkOnColumns :: [RawSQL ()] -> Maybe PrimaryKey
pkOnColumns [] = Nothing
pkOnColumns columns = Just . PrimaryKey . toNubList $ columns

pkName :: RawSQL () -> RawSQL ()
pkName tname = mconcat ["pk__", tname]

pkColumns :: PrimaryKey -> [RawSQL ()]
pkColumns (PrimaryKey columns) = fromNubList columns

sqlAddPK :: RawSQL () -> PrimaryKey -> RawSQL ()
sqlAddPK tname (PrimaryKey columns) =
  smconcat
    [ "ADD CONSTRAINT"
    , pkName tname
    , "PRIMARY KEY ("
    , mintercalate ", " $ fromNubList columns
    , ")"
    ]

-- | Convert a unique index into a primary key. Main usage is to build a unique
-- index concurrently first (so that its creation doesn't conflict with table
-- updates on the modified table) and then convert it into a primary key using
-- this function.
sqlAddPKUsing :: RawSQL () -> TableIndex -> RawSQL ()
sqlAddPKUsing tname idx =
  smconcat
    [ "ADD CONSTRAINT"
    , pkName tname
    , "PRIMARY KEY USING INDEX"
    , indexName tname idx
    ]

sqlDropPK :: RawSQL () -> RawSQL ()
sqlDropPK tname = "DROP CONSTRAINT" <+> pkName tname
