module Database.PostgreSQL.PQTypes.Model.CompositeType
  ( CompositeType (..)
  , CompositeColumn (..)
  , compositeTypePqFormat
  , sqlCreateComposite
  , sqlDropComposite
  , getDBCompositeTypes
  ) where

import Data.ByteString qualified as BS
import Data.Int
import Data.Monoid.Utils
import Data.Text.Encoding qualified as T
import Database.PostgreSQL.PQTypes

import Database.PostgreSQL.PQTypes.Model.ColumnType
import Database.PostgreSQL.PQTypes.SQL.Builder

data CompositeType = CompositeType
  { ctName :: !(RawSQL ())
  , ctColumns :: ![CompositeColumn]
  }
  deriving (Eq, Ord, Show)

data CompositeColumn = CompositeColumn
  { ccName :: !(RawSQL ())
  , ccType :: ColumnType
  }
  deriving (Eq, Ord, Show)

-- | Convenience function for converting CompositeType definition to
-- corresponding 'pqFormat' definition.
compositeTypePqFormat :: CompositeType -> BS.ByteString
compositeTypePqFormat ct = "%" `BS.append` T.encodeUtf8 (unRawSQL $ ctName ct)

-- | Make SQL query that creates a composite type.
sqlCreateComposite :: CompositeType -> RawSQL ()
sqlCreateComposite CompositeType {..} =
  smconcat
    [ "CREATE TYPE"
    , ctName
    , "AS ("
    , mintercalate ", " $ map columnToSQL ctColumns
    , ")"
    ]
  where
    columnToSQL CompositeColumn {..} = ccName <+> columnTypeToSQL ccType

-- | Make SQL query that drops a composite type.
sqlDropComposite :: RawSQL () -> RawSQL ()
sqlDropComposite = ("DROP TYPE" <+>)

----------------------------------------

-- | Get composite types defined in the database.
getDBCompositeTypes :: forall m. MonadDB m => m [CompositeType]
getDBCompositeTypes = do
  runQuery_ . sqlSelect "pg_catalog.pg_class c" $ do
    sqlResult "c.relname::text"
    sqlResult "c.oid::int4"
    sqlWhere "pg_catalog.pg_table_is_visible(c.oid)"
    sqlWhereEq "c.relkind" 'c'
    sqlOrderBy "c.relname"
  mapM getComposite =<< fetchMany id
  where
    getComposite :: (String, Int32) -> m CompositeType
    getComposite (name, oid) = do
      runQuery_ . sqlSelect "pg_catalog.pg_attribute a" $ do
        sqlResult "a.attname::text"
        sqlResult "pg_catalog.format_type(a.atttypid, a.atttypmod)"
        sqlWhereEq "a.attrelid" oid
        sqlOrderBy "a.attnum"
      columns <- fetchMany fetch
      return CompositeType {ctName = unsafeSQL name, ctColumns = columns}
      where
        fetch :: (String, ColumnType) -> CompositeColumn
        fetch (cname, ctype) =
          CompositeColumn {ccName = unsafeSQL cname, ccType = ctype}
