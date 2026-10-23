{-# LANGUAGE CPP #-}

-- |
--
-- Using migrations is fairly easy. After you've defined the lists of
-- migrations and tables, just run
-- 'Database.PostgreSQL.PQTypes.Checks.migrateDatabase':
--
-- @
-- definitions = emptyDbDefinitions { ... }
--
-- migrations :: [Migration]
-- migrations = ...
--
-- migrateDatabase options definitions migrations
-- @
--
-- Migrations are run strictly in the order specified in the migrations
-- list, starting with the first migration for which the corresponding
-- table in the DB has the version number equal to the 'mgrFrom' field of
-- the migration.
module Database.PostgreSQL.PQTypes.Model.Migration
  ( DropTableMode (..)
  , MigrationAction (..)
  , Migration (..)
  , isStandardMigration
  , isDropTableMigration
  ) where

import Data.Int

import Database.PostgreSQL.PQTypes.FromRow (FromRow)
import Database.PostgreSQL.PQTypes.Model.Index
import Database.PostgreSQL.PQTypes.Model.Table
import Database.PostgreSQL.PQTypes.SQL (SQL)
import Database.PostgreSQL.PQTypes.SQL.Raw

-- | Migration action to run, either an arbitrary 'MonadDB' action, or
-- something more fine-grained.
data MigrationAction m
  = -- | Standard migration, i.e. an arbitrary 'MonadDB' action.
    StandardMigration (m ())
  | -- | Drop table migration. Parameter is the drop table mode
    -- (@RESTRICT@/@CASCADE@). The 'Migration' record holds the name of
    -- the table to drop.
    DropTableMigration DropTableMode
  | -- | Migration for creating an index concurrently.
    CreateIndexConcurrentlyMigration
      (Maybe (RawSQL ()))
      -- ^ Optional name of the local index that, if exists, will be renamed to
      -- the desired index instead of creating it from scratch.
      --
      -- - If the index doesn't exist, the migration will proceed as usual and
      --  create the index from scratch.
      --
      -- - If the index exists and its structure doesn't match the desired
      --   index, the migration will abort with an error.
      TableIndex
      -- ^ Index
  | -- | Migration for dropping an index concurrently.
    DropIndexConcurrentlyMigration
      TableIndex
      -- ^ Index
  | -- | Migration for modifying columns. Parameters are:
    --
    -- SQL providing a list of primary keys from the associated table that will be used for the cursor.
    --
    -- Function that takes a batch of primary keys provided by the cursor SQL and runs an arbitrary computation
    -- within MonadDB. The function might be called repeatedly depending on the batch size and total number of
    -- selected primary keys. See the last argument.
    --
    -- Batch size of primary keys to be fetched at once by the cursor SQL and be given to the modification function.
    -- To handle multi-column primary keys, the following needs to be done:
    --
    --   1. Get the list of tuples from PostgreSQL.
    --   2. Unzip them into a tuple of lists in Haskell.
    --   3. Pass the lists to PostgreSQL as separate parameters and zip them back in the SQL,
    --      see https://stackoverflow.com/questions/12414750/is-there-something-like-a-zip-function-in-postgresql-that-combines-two-arrays for more details.
    forall t. FromRow t => ModifyColumnMigration SQL ([t] -> m ()) Int

-- | Migration object.
data Migration m
  = Migration
  { mgrTableName :: RawSQL ()
  -- ^ The name of the table you're migrating.
  , mgrFrom :: Int32
  -- ^ The version you're migrating *from* (you don't specify what
  -- version you migrate TO, because version is always increased by 1,
  -- so if 'mgrFrom' is 2, that means that after that migration is run,
  -- table version will equal 3
  , mgrAction :: MigrationAction m
  -- ^ Migration action.
  }

isStandardMigration :: Migration m -> Bool
isStandardMigration Migration {..} =
  case mgrAction of
    StandardMigration {} -> True
    DropTableMigration {} -> False
    CreateIndexConcurrentlyMigration {} -> False
    DropIndexConcurrentlyMigration {} -> False
    ModifyColumnMigration {} -> False

isDropTableMigration :: Migration m -> Bool
isDropTableMigration Migration {..} =
  case mgrAction of
    StandardMigration {} -> False
    DropTableMigration {} -> True
    CreateIndexConcurrentlyMigration {} -> False
    DropIndexConcurrentlyMigration {} -> False
    ModifyColumnMigration {} -> False
