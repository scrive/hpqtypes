module Database.PostgreSQL.PQTypes.Model.Table
  ( TableColumn (..)
  , tblColumn
  , sqlAddColumn
  , sqlAlterColumn
  , sqlDropColumn
  , Rows (..)
  , Table (..)
  , tblTable
  , sqlCreateTable
  , sqlAlterTable
  , DropTableMode (..)
  , sqlDropTable
  , TableInitialSetup (..)
  ) where

import Control.Monad.Catch
import Data.ByteString (ByteString)
import Data.Int
import Data.Monoid.Utils
import Database.PostgreSQL.PQTypes

import Database.PostgreSQL.PQTypes.Model.Check
import Database.PostgreSQL.PQTypes.Model.ColumnType
import Database.PostgreSQL.PQTypes.Model.ForeignKey
import Database.PostgreSQL.PQTypes.Model.Index
import Database.PostgreSQL.PQTypes.Model.PrimaryKey
import Database.PostgreSQL.PQTypes.Model.Trigger

data TableColumn = TableColumn
  { colName :: RawSQL ()
  , colType :: ColumnType
  , colCollation :: Maybe (RawSQL ())
  , colNullable :: Bool
  , colDefault :: Maybe (RawSQL ())
  }
  deriving (Show)

tblColumn :: TableColumn
tblColumn =
  TableColumn
    { colName = error "tblColumn: column name must be specified"
    , colType = error "tblColumn: column type must be specified"
    , colCollation = Nothing
    , colNullable = True
    , colDefault = Nothing
    }

sqlAddColumn :: TableColumn -> RawSQL ()
sqlAddColumn TableColumn {..} =
  smconcat
    [ "ADD COLUMN"
    , colName
    , columnTypeToSQL colType
    , maybe "" (\c -> "COLLATE \"" <> c <> "\"") colCollation
    , if colNullable then "NULL" else "NOT NULL"
    , maybe "" ("DEFAULT" <+>) colDefault
    ]

sqlAlterColumn :: RawSQL () -> RawSQL () -> RawSQL ()
sqlAlterColumn cname alter = "ALTER COLUMN" <+> cname <+> alter

sqlDropColumn :: RawSQL () -> RawSQL ()
sqlDropColumn cname = "DROP COLUMN" <+> cname

----------------------------------------

data Rows = forall row. (Show row, ToRow row) => Rows [ByteString] [row]

data Table
  = Table
  { tblName :: RawSQL ()
  -- ^ Must be in lower case.
  , tblVersion :: Int32
  , tblColumns :: [TableColumn]
  , tblPrimaryKey :: Maybe PrimaryKey
  , tblChecks :: [Check]
  , tblForeignKeys :: [ForeignKey]
  , tblIndexes :: [TableIndex]
  , tblTriggers :: [Trigger]
  , tblInitialSetup :: Maybe TableInitialSetup
  }

data TableInitialSetup = TableInitialSetup
  { checkInitialSetup :: forall m. (MonadDB m, MonadThrow m) => m Bool
  , initialSetup :: forall m. (MonadDB m, MonadThrow m) => m ()
  }

tblTable :: Table
tblTable =
  Table
    { tblName = error "tblTable: table name must be specified"
    , tblVersion = error "tblTable: table version must be specified"
    , tblColumns = error "tblTable: table columns must be specified"
    , tblPrimaryKey = Nothing
    , tblChecks = []
    , tblForeignKeys = []
    , tblIndexes = []
    , tblTriggers = []
    , tblInitialSetup = Nothing
    }

sqlCreateTable :: RawSQL () -> RawSQL ()
sqlCreateTable tname = "CREATE TABLE" <+> tname <+> "()"

-- | Whether to also drop objects that depend on the table.
data DropTableMode
  = -- | Automatically drop objects that depend on the table (such as views).
    DropTableCascade
  | -- | Refuse to drop the table if any objects depend on it. This is the default.
    DropTableRestrict

sqlDropTable :: RawSQL () -> DropTableMode -> RawSQL ()
sqlDropTable tname mode =
  "DROP TABLE"
    <+> tname
    <+> case mode of
      DropTableCascade -> "CASCADE"
      DropTableRestrict -> "RESTRICT"

sqlAlterTable :: RawSQL () -> [RawSQL ()] -> RawSQL ()
sqlAlterTable tname alter_statements =
  smconcat
    [ "ALTER TABLE"
    , tname
    , mintercalate ", " alter_statements
    ]
