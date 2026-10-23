module Database.PostgreSQL.PQTypes.Migrate
  ( createDomain
  , createTable
  , createTableConstraints
  , createTableTriggers
  ) where

import Control.Monad
import Data.Foldable qualified as F

import Database.PostgreSQL.PQTypes
import Database.PostgreSQL.PQTypes.Checks.Util
import Database.PostgreSQL.PQTypes.Model
import Database.PostgreSQL.PQTypes.SQL.Builder

createDomain :: MonadDB m => Domain -> m ()
createDomain dom@Domain {..} = do
  -- create the domain
  runQuery_ $ sqlCreateDomain dom
  -- add constraint checks to the domain
  F.forM_ domChecks $ runQuery_ . sqlAlterDomain domName . sqlAddValidCheckMaybeDowntime

createTable :: MonadDB m => Bool -> Table -> m ()
createTable withConstraints table@Table {..} = do
  -- Create empty table and add the columns.
  runQuery_ $ sqlCreateTable tblName
  runQuery_ $ sqlAlterTable tblName $ map sqlAddColumn tblColumns
  -- Add the primary key if applicable.
  forM_ tblPrimaryKey $ \pk -> runQuery_ $ sqlAlterTable tblName [sqlAddPK tblName pk]
  -- Add indexes.
  forM_ tblIndexes $ runQuery_ . sqlCreateIndexMaybeDowntime tblName
  -- Add all the other constraints if applicable.
  when withConstraints $ createTableConstraints table
  -- Create triggers.
  createTableTriggers table
  -- Register the table along with its version.
  runQuery_ . sqlInsert "table_versions" $ do
    sqlSet "name" (tblNameText table)
    sqlSet "version" tblVersion

createTableConstraints :: MonadDB m => Table -> m ()
createTableConstraints Table {..} = unless (null addConstraints) $ do
  runQuery_ $ sqlAlterTable tblName addConstraints
  where
    addConstraints =
      map sqlAddValidCheckMaybeDowntime tblChecks
        ++ map (sqlAddValidFKMaybeDowntime tblName) tblForeignKeys

createTableTriggers :: MonadDB m => Table -> m ()
createTableTriggers = mapM_ createTrigger . tblTriggers
