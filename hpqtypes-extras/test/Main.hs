{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use head" #-}
module Main where

import Control.Monad (forM_)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Either
import Data.List (zip4)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.UUID.Types
import System.Environment
import System.Exit

import Data.Monoid.Utils
import Database.PostgreSQL.PQTypes
import Database.PostgreSQL.PQTypes.Checks
import Database.PostgreSQL.PQTypes.Model
import Database.PostgreSQL.PQTypes.SQL.Builder
import Log
import Log.Backend.StandardOutput

import Data.Map qualified as M
import Database.PostgreSQL.PQTypes.Model.Function
import Test.Tasty
import Test.Tasty.HUnit

testExtrasOptions :: ExtrasOptions
testExtrasOptions =
  defaultExtrasOptions
    { eoCheckOverlappingIndexes = True
    }

-- Simple example schemata inspired by the one in
-- <  http://www.databaseanswers.org/data_models/bank_robberies/index.htm>
--
-- Schema 1: Bank robberies, tables:
--           bank, bad_guy, robbery, participated_in_robbery, witness,
--           witnessed_robbery
-- Schema 2: Witnesses go into witness protection program,
--           (some) bad guys get arrested:
--           drop tables witness and witnessed_robbery,
--           add table under_arrest
-- Schema 3: Bad guys get their prison sentences:
--           drop table under_arrest
--           add table prison_sentence
-- Schema 4: New 'cash' column for the 'bank' table.
-- Schema 5: Create a new table 'flash',
--           drop the 'cash' column from the 'bank' table,
--           drop table 'flash'.
-- Cleanup:  drop everything

tableBankSchema1 :: Table
tableBankSchema1 =
  tblTable
    { tblName = "bank"
    , tblVersion = 1
    , tblColumns =
        [ tblColumn
            { colName = "id"
            , colType = UuidT
            , colNullable = False
            , colDefault = Just "gen_random_uuid()"
            }
        , tblColumn
            { colName = "name"
            , colType = TextT
            , colCollation = Just "en_US"
            , colNullable = False
            }
        , tblColumn
            { colName = "location"
            , colType = TextT
            , colCollation = Just "C"
            , colNullable = False
            }
        ]
    , tblPrimaryKey = pkOnColumn "id"
    , tblTriggers = []
    }

tableBankSchema2 :: Table
tableBankSchema2 = tableBankSchema1

tableBankSchema3 :: Table
tableBankSchema3 = tableBankSchema2

tableBankMigration4 :: MonadDB m => Migration m
tableBankMigration4 =
  Migration
    { mgrTableName = tblName tableBankSchema3
    , mgrFrom = 1
    , mgrAction = StandardMigration $ do
        runQuery_ $
          sqlAlterTable
            (tblName tableBankSchema3)
            [ sqlAddColumn $
                tblColumn
                  { colName = "cash"
                  , colType = IntegerT
                  , colNullable = False
                  , colDefault = Just "0"
                  }
            ]
    }

tableBankSchema4 :: Table
tableBankSchema4 =
  tableBankSchema3
    { tblVersion = tblVersion tableBankSchema3 + 1
    , tblColumns =
        tblColumns tableBankSchema3
          ++ [ tblColumn
                 { colName = "cash"
                 , colType = IntegerT
                 , colNullable = False
                 , colDefault = Just "0"
                 }
             ]
    }

tableBankMigration5fst :: MonadDB m => Migration m
tableBankMigration5fst =
  Migration
    { mgrTableName = tblName tableBankSchema3
    , mgrFrom = 2
    , mgrAction = StandardMigration $ do
        runQuery_ $
          sqlAlterTable
            (tblName tableBankSchema4)
            [ sqlDropColumn "cash"
            ]
    }

tableBankMigration5snd :: MonadDB m => Migration m
tableBankMigration5snd =
  Migration
    { mgrTableName = tblName tableBankSchema3
    , mgrFrom = 3
    , mgrAction =
        CreateIndexConcurrentlyMigration
          (Just "local_idx_doesnt_exist")
          ((indexOnColumn "name") {idxInclude = ["id", "location"]})
    }

tableBankMigration5thrd :: MonadDB m => Migration m
tableBankMigration5thrd =
  Migration
    { mgrTableName = tblName tableBankSchema3
    , mgrFrom = 4
    , mgrAction =
        CreateIndexConcurrentlyMigration
          (Just localBankIndexToRename)
          (indexOnColumns ["id", "name"])
    }

tableBankSchema5 :: Table
tableBankSchema5 =
  tableBankSchema4
    { tblVersion = tblVersion tableBankSchema4 + 3
    , tblColumns =
        filter
          (\c -> colName c /= "cash")
          (tblColumns tableBankSchema4)
    , tblIndexes =
        [ (indexOnColumn "name") {idxInclude = ["id", "location"]}
        , indexOnColumns ["id", "name"]
        ]
    }

tableBadGuySchema1 :: Table
tableBadGuySchema1 =
  tblTable
    { tblName = "bad_guy"
    , tblVersion = 1
    , tblColumns =
        [ tblColumn
            { colName = "id"
            , colType = UuidT
            , colNullable = False
            , colDefault = Just "gen_random_uuid()"
            }
        , tblColumn
            { colName = "firstname"
            , colType = TextT
            , colNullable = False
            }
        , tblColumn
            { colName = "lastname"
            , colType = TextT
            , colNullable = False
            }
        ]
    , tblPrimaryKey = pkOnColumn "id"
    }

tableBadGuySchema2 :: Table
tableBadGuySchema2 = tableBadGuySchema1

tableBadGuySchema3 :: Table
tableBadGuySchema3 = tableBadGuySchema2

tableBadGuySchema4 :: Table
tableBadGuySchema4 = tableBadGuySchema3

tableBadGuySchema5 :: Table
tableBadGuySchema5 = tableBadGuySchema4

tableRobberySchema1 :: Table
tableRobberySchema1 =
  tblTable
    { tblName = "robbery"
    , tblVersion = 1
    , tblColumns =
        [ tblColumn
            { colName = "id"
            , colType = UuidT
            , colNullable = False
            , colDefault = Just "gen_random_uuid()"
            }
        , tblColumn
            { colName = "bank_id"
            , colType = UuidT
            , colNullable = False
            }
        , tblColumn
            { colName = "date"
            , colType = DateT
            , colNullable = False
            , colDefault = Just "now()"
            }
        ]
    , tblPrimaryKey = pkOnColumn "id"
    , tblForeignKeys = [fkOnColumn "bank_id" "bank" "id"]
    }

tableRobberySchema2 :: Table
tableRobberySchema2 = tableRobberySchema1

tableRobberySchema3 :: Table
tableRobberySchema3 = tableRobberySchema2

tableRobberySchema4 :: Table
tableRobberySchema4 = tableRobberySchema3

tableRobberySchema5 :: Table
tableRobberySchema5 = tableRobberySchema4

tableParticipatedInRobberySchema1 :: Table
tableParticipatedInRobberySchema1 =
  tblTable
    { tblName = "participated_in_robbery"
    , tblVersion = 1
    , tblColumns =
        [ tblColumn
            { colName = "bad_guy_id"
            , colType = UuidT
            , colNullable = False
            }
        , tblColumn
            { colName = "robbery_id"
            , colType = UuidT
            , colNullable = False
            }
        ]
    , tblPrimaryKey = pkOnColumns ["bad_guy_id", "robbery_id"]
    , tblForeignKeys =
        [ fkOnColumn "bad_guy_id" "bad_guy" "id"
        , fkOnColumn "robbery_id" "robbery" "id"
        ]
    }

tableParticipatedInRobberySchema2 :: Table
tableParticipatedInRobberySchema2 = tableParticipatedInRobberySchema1

tableParticipatedInRobberySchema3 :: Table
tableParticipatedInRobberySchema3 = tableParticipatedInRobberySchema2

tableParticipatedInRobberySchema4 :: Table
tableParticipatedInRobberySchema4 = tableParticipatedInRobberySchema3

tableParticipatedInRobberySchema5 :: Table
tableParticipatedInRobberySchema5 = tableParticipatedInRobberySchema4

tableWitnessName :: RawSQL ()
tableWitnessName = "witness"

tableWitnessSchema1 :: Table
tableWitnessSchema1 =
  tblTable
    { tblName = tableWitnessName
    , tblVersion = 1
    , tblColumns =
        [ tblColumn
            { colName = "id"
            , colType = UuidT
            , colNullable = False
            , colDefault = Just "gen_random_uuid()"
            }
        , tblColumn
            { colName = "firstname"
            , colType = TextT
            , colNullable = False
            }
        , tblColumn
            { colName = "lastname"
            , colType = TextT
            , colNullable = False
            }
        ]
    , tblPrimaryKey = pkOnColumn "id"
    }

tableWitnessedRobberyName :: RawSQL ()
tableWitnessedRobberyName = "witnessed_robbery"

tableWitnessedRobberySchema1 :: Table
tableWitnessedRobberySchema1 =
  tblTable
    { tblName = tableWitnessedRobberyName
    , tblVersion = 1
    , tblColumns =
        [ tblColumn
            { colName = "witness_id"
            , colType = UuidT
            , colNullable = False
            }
        , tblColumn
            { colName = "robbery_id"
            , colType = UuidT
            , colNullable = False
            }
        ]
    , tblPrimaryKey = pkOnColumns ["witness_id", "robbery_id"]
    , tblForeignKeys =
        [ fkOnColumn "witness_id" "witness" "id"
        , fkOnColumn "robbery_id" "robbery" "id"
        ]
    }

tableUnderArrestName :: RawSQL ()
tableUnderArrestName = "under_arrest"

tableUnderArrestSchema2 :: Table
tableUnderArrestSchema2 =
  tblTable
    { tblName = tableUnderArrestName
    , tblVersion = 1
    , tblColumns =
        [ tblColumn
            { colName = "bad_guy_id"
            , colType = UuidT
            , colNullable = False
            }
        , tblColumn
            { colName = "robbery_id"
            , colType = UuidT
            , colNullable = False
            }
        , tblColumn
            { colName = "court_date"
            , colType = DateT
            , colNullable = False
            , colDefault = Just "now()"
            }
        ]
    , tblPrimaryKey = pkOnColumns ["bad_guy_id", "robbery_id"]
    , tblForeignKeys =
        [ fkOnColumn "bad_guy_id" "bad_guy" "id"
        , fkOnColumn "robbery_id" "robbery" "id"
        ]
    }

tablePrisonSentenceName :: RawSQL ()
tablePrisonSentenceName = "prison_sentence"

tablePrisonSentenceSchema3 :: Table
tablePrisonSentenceSchema3 =
  tblTable
    { tblName = tablePrisonSentenceName
    , tblVersion = 1
    , tblColumns =
        [ tblColumn
            { colName = "bad_guy_id"
            , colType = UuidT
            , colNullable = False
            }
        , tblColumn
            { colName = "robbery_id"
            , colType = UuidT
            , colNullable = False
            }
        , tblColumn
            { colName = "sentence_start"
            , colType = DateT
            , colNullable = False
            , colDefault = Just "now()"
            }
        , tblColumn
            { colName = "sentence_length"
            , colType = IntegerT
            , colNullable = False
            , colDefault = Just "6"
            }
        , tblColumn
            { colName = "prison_name"
            , colType = TextT
            , colNullable = False
            }
        ]
    , tblPrimaryKey = pkOnColumns ["bad_guy_id", "robbery_id"]
    , tblForeignKeys =
        [ fkOnColumn "bad_guy_id" "bad_guy" "id"
        , fkOnColumn "robbery_id" "robbery" "id"
        ]
    }

tablePrisonSentenceSchema4 :: Table
tablePrisonSentenceSchema4 = tablePrisonSentenceSchema3

tablePrisonSentenceSchema5 :: Table
tablePrisonSentenceSchema5 = tablePrisonSentenceSchema4

tableFlashName :: RawSQL ()
tableFlashName = "flash"

tableFlash :: Table
tableFlash =
  tblTable
    { tblName = tableFlashName
    , tblVersion = 1
    , tblColumns =
        [ tblColumn {colName = "flash_id", colType = UuidT, colNullable = False}
        ]
    }

tableCartelName :: RawSQL ()
tableCartelName = "cartel"

tableCartel :: Table
tableCartel =
  tblTable
    { tblName = tableCartelName
    , tblVersion = 1
    , tblColumns =
        [ tblColumn
            { colName = "cartel_member_id"
            , colType = UuidT
            , colNullable = False
            }
        , tblColumn
            { colName = "cartel_boss_id"
            , colType = UuidT
            , colNullable = True
            }
        ]
    , tblPrimaryKey = pkOnColumns ["cartel_member_id"]
    , tblForeignKeys =
        [ fkOnColumn "cartel_member_id" "bad_guy" "id"
        , fkOnColumn "cartel_boss_id" "bad_guy" "id"
        ]
    }

tableCartelSchema1 :: Table
tableCartelSchema1 = tableCartel

createTableMigration :: MonadDB m => Table -> Migration m
createTableMigration tbl =
  Migration
    { mgrTableName = tblName tbl
    , mgrFrom = 0
    , mgrAction = StandardMigration $ do
        createTable True tbl
    }

dropTableMigration :: MonadDB m => Table -> Migration m
dropTableMigration tbl =
  Migration
    { mgrTableName = tblName tbl
    , mgrFrom = tblVersion tbl
    , mgrAction = DropTableMigration DropTableCascade
    }

schema1Tables :: [Table]
schema1Tables =
  [ tableBankSchema1
  , tableBadGuySchema1
  , tableRobberySchema1
  , tableParticipatedInRobberySchema1
  , tableWitnessSchema1
  , tableWitnessedRobberySchema1
  ]

schema1Migrations :: MonadDB m => [Migration m]
schema1Migrations =
  [ createTableMigration tableBankSchema1
  , createTableMigration tableBadGuySchema1
  , createTableMigration tableRobberySchema1
  , createTableMigration tableParticipatedInRobberySchema1
  , createTableMigration tableWitnessSchema1
  , createTableMigration tableWitnessedRobberySchema1
  ]

schema2Tables :: [Table]
schema2Tables =
  [ tableBankSchema2
  , tableBadGuySchema2
  , tableRobberySchema2
  , tableParticipatedInRobberySchema2
  , tableUnderArrestSchema2
  ]

schema2Migrations :: MonadDB m => [Migration m]
schema2Migrations =
  schema1Migrations
    ++ [ dropTableMigration tableWitnessedRobberySchema1
       , dropTableMigration tableWitnessSchema1
       , createTableMigration tableUnderArrestSchema2
       ]

schema3Tables :: [Table]
schema3Tables =
  [ tableBankSchema3
  , tableBadGuySchema3
  , tableRobberySchema3
  , tableParticipatedInRobberySchema3
  , tablePrisonSentenceSchema3
  ]

schema3Migrations :: MonadDB m => [Migration m]
schema3Migrations =
  schema2Migrations
    ++ [ dropTableMigration tableUnderArrestSchema2
       , createTableMigration tablePrisonSentenceSchema3
       ]

schema4Tables :: [Table]
schema4Tables =
  [ tableBankSchema4
  , tableBadGuySchema4
  , tableRobberySchema4
  , tableParticipatedInRobberySchema4
  , tablePrisonSentenceSchema4
  ]

schema4Migrations :: MonadDB m => [Migration m]
schema4Migrations =
  schema3Migrations
    ++ [tableBankMigration4]

schema5Tables :: [Table]
schema5Tables =
  [ tableBankSchema5
  , tableBadGuySchema5
  , tableRobberySchema5
  , tableParticipatedInRobberySchema5
  , tablePrisonSentenceSchema5
  ]

schema5Migrations :: MonadDB m => [Migration m]
schema5Migrations =
  schema4Migrations
    ++ [ createTableMigration tableFlash
       , tableBankMigration5fst
       , tableBankMigration5snd
       , tableBankMigration5thrd
       , dropTableMigration tableFlash
       ]

schema6Tables :: [Table]
schema6Tables =
  [ tableBankSchema1
  , tableBadGuySchema1
  , tableRobberySchema1
  , tableParticipatedInRobberySchema1
      { tblVersion = tblVersion tableParticipatedInRobberySchema1 + 1
      , tblPrimaryKey = Nothing
      }
  , tableWitnessSchema1
  , tableWitnessedRobberySchema1
  ]

schema6Migrations :: MonadDB m => Migration m
schema6Migrations =
  Migration
    { mgrTableName = tblName tableParticipatedInRobberySchema1
    , mgrFrom = tblVersion tableParticipatedInRobberySchema1
    , mgrAction =
        StandardMigration $ do
          runQuery_
            ( "ALTER TABLE participated_in_robbery DROP CONSTRAINT \
              \pk__participated_in_robbery"
                :: RawSQL ()
            )
    }

type TestM a = DBT (LogT IO) a

localBankIndexToRename :: RawSQL ()
localBankIndexToRename = "local_idx_bank_id_name"

createTablesSchema1 :: (String -> TestM ()) -> TestM ()
createTablesSchema1 step = do
  let definitions = tableDefsWithPgCrypto schema1Tables
  step "Creating the database (schema version 1)..."
  migrateDatabase testExtrasOptions definitions schema1Migrations

  -- Add local indexes that shouldn't trigger validation (in particular overlap,
  -- both "is included in" and "includes" case) errors.
  runSQL_ "CREATE INDEX local_idx__bank__id ON bank(id)"
  runSQL_ "CREATE INDEX local_idx__bank__id__name__location ON bank(id,name,location)"

  -- Add a local index that will be renamed during a migration later.
  runQuery_ $ "CREATE INDEX" <+> localBankIndexToRename <+> "ON bank(id, name)"

  checkDatabase testExtrasOptions definitions

testDBSchema1 :: (String -> TestM ()) -> TestM ([UUID], [UUID])
testDBSchema1 step = do
  step "Running test queries (schema version 1)..."

  -- Populate the 'bank' table.
  runQuery_ . sqlInsert "bank" $ do
    sqlSetList
      "name"
      [ "HSBC" :: T.Text
      , "Swedbank"
      , "Nordea"
      , "Citi"
      , "Wells Fargo"
      ]
    sqlSetList
      "location"
      [ "13 Foo St., Tucson, AZ, USa" :: T.Text
      , "18 Bargatan, Stockholm, Sweden"
      , "23 Baz Lane, Liverpool, UK"
      , "2/3 Quux Ave., Milton Keynes, UK"
      , "6600 Sunset Blvd., Los Angeles, CA, USA"
      ]
    sqlResult "id"
  (bankIds :: [UUID]) <- fetchMany runIdentity
  liftIO $ assertEqual "INSERT into 'bank' table" 5 (length bankIds)

  -- Try to insert with existing ID to check that ON CONFLICT works properly
  let bankId = head bankIds
      name = "Santander" :: T.Text
      location = "Spain" :: T.Text

  runQuery_ . sqlInsert "bank" $ do
    sqlSet "id" bankId
    sqlSet "name" name
    sqlSet "location" location
    sqlOnConflictOnColumns ["id"] . sqlUpdate "" $ do
      sqlSet "name" name
      sqlSet "location" location
  runQuery_ . sqlSelect "bank" $ do
    sqlResult "name"
    sqlResult "location"
    sqlWhereEq "id" bankId
  details1 <- fetchOne id
  liftIO $ assertEqual "INSERT ON CONFLICT updates" (name, location) details1

  runQuery_ . sqlInsert "bank" $ do
    sqlSet "id" bankId
    sqlSet "name" ("" :: T.Text)
    sqlSet "location" ("" :: T.Text)
    sqlOnConflictDoNothing
  runQuery_ . sqlSelect "bank" $ do
    sqlResult "name"
    sqlResult "location"
    sqlWhereEq "id" bankId
  details3 <- fetchOne id
  liftIO $ assertEqual "INSERT ON CONFLICT does nothing (1)" (name, location) details3

  runQuery_ . sqlInsert "bank" $ do
    sqlSet "id" bankId
    sqlSet "name" ("" :: T.Text)
    sqlSet "location" ("" :: T.Text)
    sqlOnConflictOnColumnsDoNothing ["id"]
  runQuery_ . sqlSelect "bank" $ do
    sqlResult "name"
    sqlResult "location"
    sqlWhereEq "id" bankId
  details4 <- fetchOne id
  liftIO $ assertEqual "INSERT ON CONFLICT does nothing (2)" (name, location) details4

  -- If NO CONFLICT is not specified, make sure we throw an exception.
  eres :: Either DBException () <- try . withSavepoint "testDBSchema" $ do
    runQuery_ . sqlInsert "bank" $ do
      sqlSet "id" bankId
      sqlSet "name" name
      sqlSet "location" location
  liftIO $ assertBool "If ON CONFLICT is not specified an exception is thrown" (isLeft eres)

  runQuery_ . sqlInsertSelect "bank" "bank" $ do
    sqlSetCmd "id" "id"
    sqlSetCmd "name" "name"
    sqlSetCmd "location" "location"
    sqlWhereEq "id" bankId
    sqlOnConflictOnColumns ["id"] . sqlUpdate "" $ do
      sqlSet "name" name
      sqlSet "location" location
  runQuery_ . sqlSelect "bank" $ do
    sqlResult "name"
    sqlResult "location"
    sqlWhereEq "id" bankId
  details5 <- fetchOne id
  liftIO $ assertEqual "INSERT ON CONFLICT updates" (name, location) details5

  runQuery_ . sqlInsertSelect "bank" "bank" $ do
    sqlSetCmd "id" "id"
    sqlSetCmd "name" "name"
    sqlSetCmd "location" "location"
    sqlWhereEq "id" bankId
    sqlOnConflictDoNothing
  runQuery_ . sqlSelect "bank" $ do
    sqlResult "name"
    sqlResult "location"
    sqlWhereEq "id" bankId
  details6 <- fetchOne id
  liftIO $ assertEqual "INSERT ON CONFLICT does nothing (1)" (name, location) details6

  runQuery_ . sqlInsertSelect "bank" "bank" $ do
    sqlSetCmd "id" "id"
    sqlSetCmd "name" "name"
    sqlSetCmd "location" "location"
    sqlWhereEq "id" bankId
    sqlOnConflictOnColumnsDoNothing ["id"]
  runQuery_ . sqlSelect "bank" $ do
    sqlResult "name"
    sqlResult "location"
    sqlWhereEq "id" bankId
  details7 <- fetchOne id
  liftIO $ assertEqual "INSERT ON CONFLICT does nothing (2)" (name, location) details7

  -- If NO CONFLICT is not specified, make sure we throw an exception.
  eres1 :: Either DBException () <- try . withSavepoint "testDBSchema" $ do
    runQuery_ . sqlInsertSelect "bank" "bank" $ do
      sqlSetCmd "id" "id"
      sqlSetCmd "name" "name"
      sqlSetCmd "location" "location"
      sqlWhereEq "id" bankId
  liftIO $ assertBool "If ON CONFLICT is not specified an exception is thrown" (isLeft eres1)

  -- Populate the 'bad_guy' table.
  runQuery_ . sqlInsert "bad_guy" $ do
    sqlSetList
      "firstname"
      [ "Neil" :: T.Text
      , "Lee"
      , "Freddie"
      , "Frankie"
      , "James"
      , "Roy"
      ]
    sqlSetList
      "lastname"
      [ "Hetzel" :: T.Text
      , "Murray"
      , "Foreman"
      , "Fraser"
      , "Crosbie"
      , "Shaw"
      ]
    sqlResult "id"
  (badGuyIds :: [UUID]) <- fetchMany runIdentity
  liftIO $ assertEqual "INSERT into 'bad_guy' table" 6 (length badGuyIds)

  -- Populate the 'robbery' table.
  runQuery_ . sqlInsert "robbery" $ do
    sqlSetList "bank_id" [bankIds !! idx | idx <- [0, 3]]
    sqlResult "id"
  (robberyIds :: [UUID]) <- fetchMany runIdentity
  liftIO $ assertEqual "INSERT into 'robbery' table" 2 (length robberyIds)

  -- Populate the 'participated_in_robbery' table.
  runQuery_ . sqlInsert "participated_in_robbery" $ do
    sqlSetList "bad_guy_id" [badGuyIds !! idx | idx <- [0, 2]]
    sqlSet "robbery_id" (robberyIds !! 0)
    sqlResult "bad_guy_id"
  (participatorIds :: [UUID]) <- fetchMany runIdentity
  liftIO $
    assertEqual
      "INSERT into 'participated_in_robbery' table"
      2
      (length participatorIds)

  runQuery_ . sqlInsert "participated_in_robbery" $ do
    sqlSetList "bad_guy_id" [badGuyIds !! idx | idx <- [3, 4]]
    sqlSet "robbery_id" (robberyIds !! 1)
    sqlResult "bad_guy_id"
  (participatorIds' :: [UUID]) <- fetchMany runIdentity
  liftIO $
    assertEqual
      "INSERT into 'participated_in_robbery' table"
      2
      (length participatorIds')

  -- Populate the 'witness' table.
  runQuery_ . sqlInsert "witness" $ do
    sqlSetList
      "firstname"
      [ "Meredith" :: T.Text
      , "Charlie"
      , "Peter"
      , "Emun"
      , "Benedict"
      , "Erica"
      ]
    sqlSetList
      "lastname"
      [ "Vickers" :: T.Text
      , "Holloway"
      , "Weyland"
      , "Eliott"
      , "Wong"
      , "Hackett"
      ]
    sqlResult "id"
  (witnessIds :: [UUID]) <- fetchMany runIdentity
  liftIO $ assertEqual "INSERT into 'witness' table" 6 (length witnessIds)

  -- Populate the 'witnessed_robbery' table.
  runQuery_ . sqlInsert "witnessed_robbery" $ do
    sqlSetList "witness_id" [witnessIds !! idx | idx <- [0, 1]]
    sqlSet "robbery_id" (robberyIds !! 0)
    sqlResult "witness_id"
  (robberyWitnessIds :: [UUID]) <- fetchMany runIdentity
  liftIO $
    assertEqual
      "INSERT into 'witnessed_robbery' table"
      2
      (length robberyWitnessIds)

  runQuery_ . sqlInsert "witnessed_robbery" $ do
    sqlSetList "witness_id" [witnessIds !! idx | idx <- [2, 3, 4]]
    sqlSet "robbery_id" (robberyIds !! 1)
    sqlResult "witness_id"
  (robberyWitnessIds' :: [UUID]) <- fetchMany runIdentity
  liftIO $
    assertEqual
      "INSERT into 'witnessed_robbery' table"
      3
      (length robberyWitnessIds')

  -- Create a new record to test order-by case sensitivity.
  runQuery_ . sqlInsert "bank" $ do
    sqlSet "name" ("byblos bank" :: T.Text)
    sqlSet "location" ("SYRIA" :: T.Text)

  -- Check that ordering results by the "location" column uses case-sensitive
  -- sorting (since the collation method for that column is "C").
  runQuery_ . sqlSelect "bank" $ do
    sqlResult "location"
    sqlOrderBy "location"

  details8 <- fetchMany runIdentity
  liftIO $
    assertEqual
      "Using collation method \"C\" leads to case-sensitive ordering of results"
      [ "18 Bargatan, Stockholm, Sweden" :: String
      , "2/3 Quux Ave., Milton Keynes, UK"
      , "23 Baz Lane, Liverpool, UK"
      , "6600 Sunset Blvd., Los Angeles, CA, USA"
      , "SYRIA"
      , "Spain"
      ]
      details8

  -- Check that ordering results by the "name" column uses case-insensitive
  -- sorting (since the collation method for that column is "en_US").
  runQuery_ . sqlSelect "bank" $ do
    sqlResult "name"
    sqlOrderBy "name"

  details9 <- fetchMany runIdentity
  liftIO $
    assertEqual
      "Using collation method \"en_US\" leads to case-insensitive ordering of results"
      [ "byblos bank" :: String
      , "Citi"
      , "Nordea"
      , "Santander"
      , "Swedbank"
      , "Wells Fargo"
      ]
      details9

  do
    deletedRows <- runQuery . sqlDelete "witness" $ do
      sqlWhereEq "id" $ witnessIds !! 5
      sqlResult "firstname"
      sqlResult "lastname"
    liftIO $ assertEqual "DELETE FROM 'witness' table" 1 deletedRows

    deletedName <- fetchOne id
    liftIO $
      assertEqual
        "DELETE FROM 'witness' table RETURNING firstname, lastname"
        ("Erica" :: String, "Hackett" :: String)
        deletedName

  return (badGuyIds, robberyIds)

migrateDBToSchema2 :: (String -> TestM ()) -> TestM ()
migrateDBToSchema2 step = do
  let definitions = tableDefsWithPgCrypto schema2Tables
  step "Migrating the database (schema version 1 -> schema version 2)..."
  migrateDatabase
    testExtrasOptions
    definitions
    schema2Migrations
  checkDatabase testExtrasOptions definitions

-- | Hacky version of 'migrateDBToSchema2' used by 'migrationTest3'.
migrateDBToSchema2Hacky :: (String -> TestM ()) -> TestM ()
migrateDBToSchema2Hacky step = do
  let definitions = tableDefsWithPgCrypto schema2Tables
  step
    "Hackily migrating the database (schema version 1 \
    \-> schema version 2)..."
  migrateDatabase
    testExtrasOptions
    definitions
    schema2Migrations'
  checkDatabase testExtrasOptions definitions
  where
    schema2Migrations' = createTableMigration tableFlash : schema2Migrations

testDBSchema2 :: (String -> TestM ()) -> [UUID] -> [UUID] -> TestM ()
testDBSchema2 step badGuyIds robberyIds = do
  step "Running test queries (schema version 2)..."

  -- Check that table 'witness' doesn't exist.
  runSQL_ $
    "SELECT EXISTS (SELECT 1 FROM pg_tables WHERE schemaname = 'public'"
      <> " AND tablename = 'witness')"
  (witnessExists :: Bool) <- fetchOne runIdentity
  liftIO $ assertEqual "Table 'witness' doesn't exist" False witnessExists

  -- Check that table 'witnessed_robbery' doesn't exist.
  runSQL_ $
    "SELECT EXISTS (SELECT 1 FROM pg_tables WHERE schemaname = 'public'"
      <> " AND tablename = 'witnessed_robbery')"
  (witnessedRobberyExists :: Bool) <- fetchOne runIdentity
  liftIO $
    assertEqual
      "Table 'witnessed_robbery' doesn't exist"
      False
      witnessedRobberyExists

  -- Populate table 'under_arrest'.
  runQuery_ . sqlInsert "under_arrest" $ do
    sqlSetList "bad_guy_id" [badGuyIds !! idx | idx <- [0, 2]]
    sqlSet "robbery_id" (robberyIds !! 0)
    sqlResult "bad_guy_id"
  (arrestedIds :: [UUID]) <- fetchMany runIdentity
  liftIO $
    assertEqual
      "INSERT into 'under_arrest' table"
      2
      (length arrestedIds)

  runQuery_ . sqlInsert "under_arrest" $ do
    sqlSetList "bad_guy_id" [badGuyIds !! idx | idx <- [3, 4]]
    sqlSet "robbery_id" (robberyIds !! 1)
    sqlResult "bad_guy_id"
  (arrestedIds' :: [UUID]) <- fetchMany runIdentity
  liftIO $
    assertEqual
      "INSERT into 'under_arrest' table"
      2
      (length arrestedIds')

  return ()

migrateDBToSchema3 :: (String -> TestM ()) -> TestM ()
migrateDBToSchema3 step = do
  let definitions = tableDefsWithPgCrypto schema3Tables
  step "Migrating the database (schema version 2 -> schema version 3)..."
  migrateDatabase
    testExtrasOptions
    definitions
    schema3Migrations
  checkDatabase testExtrasOptions definitions

testDBSchema3 :: (String -> TestM ()) -> [UUID] -> [UUID] -> TestM ()
testDBSchema3 step badGuyIds robberyIds = do
  step "Running test queries (schema version 3)..."

  -- Check that table 'under_arrest' doesn't exist.
  runSQL_ $
    "SELECT EXISTS (SELECT 1 FROM pg_tables WHERE schemaname = 'public'"
      <> " AND tablename = 'under_arrest')"
  (underArrestExists :: Bool) <- fetchOne runIdentity
  liftIO $
    assertEqual
      "Table 'under_arrest' doesn't exist"
      False
      underArrestExists

  -- Check that the table 'prison_sentence' exists.
  runSQL_ $
    "SELECT EXISTS (SELECT 1 FROM pg_tables WHERE schemaname = 'public'"
      <> " AND tablename = 'prison_sentence')"
  (prisonSentenceExists :: Bool) <- fetchOne runIdentity
  liftIO $
    assertEqual
      "Table 'prison_sentence' does exist"
      True
      prisonSentenceExists

  -- Populate table 'prison_sentence'.
  runQuery_ . sqlInsert "prison_sentence" $ do
    sqlSetList "bad_guy_id" [badGuyIds !! idx | idx <- [0, 2]]
    sqlSet "robbery_id" (robberyIds !! 0)
    sqlSet "sentence_length" (12 :: Int)
    sqlSet "prison_name" ("Long Kesh" :: T.Text)
    sqlResult "bad_guy_id"
  (sentencedIds :: [UUID]) <- fetchMany runIdentity
  liftIO $
    assertEqual
      "INSERT into 'prison_sentence' table"
      2
      (length sentencedIds)

  runQuery_ . sqlInsert "prison_sentence" $ do
    sqlSetList "bad_guy_id" [badGuyIds !! idx | idx <- [3, 4]]
    sqlSet "robbery_id" (robberyIds !! 1)
    sqlSet "sentence_length" (9 :: Int)
    sqlSet "prison_name" ("Wormwood Scrubs" :: T.Text)
    sqlResult "bad_guy_id"
  (sentencedIds' :: [UUID]) <- fetchMany runIdentity
  liftIO $
    assertEqual
      "INSERT into 'prison_sentence' table"
      2
      (length sentencedIds')

  return ()

migrateDBToSchema4 :: (String -> TestM ()) -> TestM ()
migrateDBToSchema4 step = do
  let definitions = tableDefsWithPgCrypto schema4Tables
  step "Migrating the database (schema version 3 -> schema version 4)..."
  migrateDatabase
    testExtrasOptions
    definitions
    schema4Migrations
  checkDatabase testExtrasOptions definitions

testDBSchema4 :: (String -> TestM ()) -> TestM ()
testDBSchema4 step = do
  step "Running test queries (schema version 4)..."

  -- Check that the 'bank' table has a 'cash' column.
  runSQL_ $
    "SELECT EXISTS (SELECT 1 FROM information_schema.columns"
      <> " WHERE table_schema = 'public'"
      <> " AND table_name = 'bank'"
      <> " AND column_name = 'cash')"
  (colCashExists :: Bool) <- fetchOne runIdentity
  liftIO $
    assertEqual
      "Column 'cash' in the table 'bank' does exist"
      True
      colCashExists

  return ()

migrateDBToSchema5 :: (String -> TestM ()) -> TestM ()
migrateDBToSchema5 step = do
  let definitions = tableDefsWithPgCrypto schema5Tables
  step "Migrating the database (schema version 4 -> schema version 5)..."
  migrateDatabase testExtrasOptions definitions schema5Migrations
  checkDatabase testExtrasOptions definitions

testDBSchema5 :: (String -> TestM ()) -> TestM ()
testDBSchema5 step = do
  step "Running test queries (schema version 5)..."

  -- Check that the 'bank' table doesn't have a 'cash' column.
  runSQL_ $
    "SELECT EXISTS (SELECT 1 FROM information_schema.columns"
      <> " WHERE table_schema = 'public'"
      <> " AND table_name = 'bank'"
      <> " AND column_name = 'cash')"
  (colCashExists :: Bool) <- fetchOne runIdentity
  liftIO $
    assertEqual
      "Column 'cash' in the table 'bank' doesn't exist"
      False
      colCashExists

  -- Check that the 'flash' table doesn't exist.
  runSQL_ $
    "SELECT EXISTS (SELECT 1 FROM pg_tables WHERE schemaname = 'public'"
      <> " AND tablename = 'flash')"
  (flashExists :: Bool) <- fetchOne runIdentity
  liftIO $ assertEqual "Table 'flash' doesn't exist" False flashExists

  return ()

-- | May require 'ALTER SCHEMA public OWNER TO $user' the first time
-- you run this.
freshTestDB :: (String -> TestM ()) -> TestM ()
freshTestDB step = do
  step "Dropping the test DB schema..."
  runSQL_ "DROP SCHEMA public CASCADE"
  runSQL_ "CREATE SCHEMA public"

-- | Re-used by 'migrationTest5'.
migrationTest1Body :: (String -> TestM ()) -> TestM ()
migrationTest1Body step = do
  createTablesSchema1 step
  (badGuyIds, robberyIds) <-
    testDBSchema1 step

  migrateDBToSchema2 step
  testDBSchema2 step badGuyIds robberyIds

  migrateDBToSchema3 step
  testDBSchema3 step badGuyIds robberyIds

  migrateDBToSchema4 step
  testDBSchema4 step

  migrateDBToSchema5 step
  testDBSchema5 step

bankTrigger1 :: Trigger
bankTrigger1 =
  Trigger
    { triggerTable = "bank"
    , triggerName = "trigger_1"
    , triggerKind = TriggerConstraint NotDeferrable
    , triggerEvents = Set.fromList [TriggerInsert]
    , triggerWhen = Nothing
    , triggerFunction =
        defaultTriggerFunction "trigger_1" $
          "begin"
            <+> "  return null;"
            <+> "end;"
    }

bankTrigger2 :: Trigger
bankTrigger2 =
  bankTrigger1
    { triggerFunction =
        defaultTriggerFunction "trigger_1" $
          "begin"
            <+> "  return null;"
            <+> "end;"
    }

bankTrigger3 :: Trigger
bankTrigger3 =
  Trigger
    { triggerTable = "bank"
    , triggerName = "trigger_3"
    , triggerKind = TriggerConstraint DeferrableInitiallyDeferred
    , triggerEvents = Set.fromList [TriggerInsert, TriggerUpdateOf [unsafeSQL "location"]]
    , triggerWhen = Nothing
    , triggerFunction =
        defaultTriggerFunction "trigger_3" $
          "begin"
            <+> "  return null;"
            <+> "end;"
    }

bankTrigger4 :: Trigger
bankTrigger4 =
  Trigger
    { triggerTable = "bank"
    , triggerName = "trigger_4"
    , triggerKind = TriggerConstraint DeferrableInitiallyDeferred
    , triggerEvents = Set.fromList [TriggerInsert, TriggerUpdateOf [unsafeSQL "location"]]
    , triggerWhen = Nothing
    , triggerFunction =
        Function
          { fnName = "trigger_4_fun"
          , fnSecurity = Definer
          , fnConfigurationParameters = mempty
          , fnReturns = "trigger"
          , fnBody =
              mconcat
                [ "BEGIN"
                , "    RETURN NULL;"
                , "END;"
                ]
          }
    }

bankTrigger5 :: Trigger
bankTrigger5 =
  bankTrigger5WithWhiteSpace
    { triggerFunction =
        (triggerFunction bankTrigger5WithWhiteSpace)
          { fnBody = rawSQL (T.strip (unRawSQL (fnBody (triggerFunction bankTrigger5WithWhiteSpace)))) ()
          }
    }

bankTrigger5WithWhiteSpace :: Trigger
bankTrigger5WithWhiteSpace =
  Trigger
    { triggerTable = "bank"
    , triggerName = "trigger_5"
    , triggerKind = TriggerConstraint NotDeferrable
    , triggerEvents = Set.fromList [TriggerInsert]
    , triggerWhen = Nothing
    , triggerFunction =
        defaultTriggerFunction "trigger_5" $
          "    begin"
            <+> "  return null;"
            <+> "end;  "
    }

bankTrigger2Proper :: Trigger
bankTrigger2Proper =
  bankTrigger2 {triggerName = "trigger_2"}

testTriggers :: HasCallStack => (String -> TestM ()) -> TestM ()
testTriggers step = do
  step "Running trigger tests..."

  step "create the initial database"
  migrate [tableBankSchema1] [createTableMigration tableBankSchema1]

  do
    let msg = "checkDatabase fails if there are triggers in the database but not in the schema"
        ts =
          [ tableBankSchema1
              { tblVersion = 2
              , tblTriggers = []
              }
          ]
        ms = [createTriggerMigration 1 bankTrigger1]
    step msg
    assertException msg $ migrate ts ms

  do
    let msg = "checkDatabase fails if there are triggers in the schema but not in the database"
        ts =
          [ tableBankSchema1
              { tblVersion = 2
              , tblTriggers = [bankTrigger1]
              }
          ]
        ms = []
    triggerStep msg $ do
      assertException msg $ migrate ts ms

  do
    let msg = "test succeeds when creating a single trigger"
        ts =
          [ tableBankSchema1
              { tblVersion = 2
              , tblTriggers = [bankTrigger1]
              }
          ]
        ms = [createTriggerMigration 1 bankTrigger1]
    triggerStep msg $ do
      assertNoException msg $ migrate ts ms
      verify [bankTrigger1] True

  do
    -- Test that we always pull trigger function bodies with stripped whitespace
    -- from the database. This means that table definitions with unnecessary whitespace
    -- get nudged to remove them.
    let msg = "test succeeds validation with whitespaces in trigger definition"
        ts =
          [ tableBankSchema1
              { tblVersion = 2
              , tblTriggers = [bankTrigger5]
              }
          ]
        ms = [createTriggerMigration 1 bankTrigger5WithWhiteSpace]
    triggerStep msg $ do
      assertNoException msg $ migrate ts ms
      verify [bankTrigger5] True

  do
    -- Validates the above by checking if it correctly fails validation if
    -- whitespace is seen in a trigger Haskell definition.
    let msg = "test fails validation with whitespaces in trigger definition"
        ts =
          [ tableBankSchema1
              { tblVersion = 2
              , tblTriggers = [bankTrigger5WithWhiteSpace]
              }
          ]
        ms = [createTriggerMigration 1 bankTrigger5WithWhiteSpace]
    triggerStep msg $ do
      assertException msg $ migrate ts ms

  do
    -- Attempt to create the same triggers twice. Should fail with a DBException saying
    -- that function already exists.
    let msg = "database exception is raised if trigger is created twice"
        ts =
          [ tableBankSchema1
              { tblVersion = 3
              , tblTriggers = [bankTrigger1]
              }
          ]
        ms =
          [ createTriggerMigration 1 bankTrigger1
          , createTriggerMigration 2 bankTrigger1
          ]
    triggerStep msg $ do
      assertDBException msg $ migrate ts ms

  do
    let msg = "database exception is raised if triggers only differ in function name"
        ts =
          [ tableBankSchema1
              { tblVersion = 3
              , tblTriggers = [bankTrigger1, bankTrigger2]
              }
          ]
        ms =
          [ createTriggerMigration 1 bankTrigger1
          , createTriggerMigration 2 bankTrigger2
          ]
    triggerStep msg $ do
      assertDBException msg $ migrate ts ms

  do
    let msg = "successfully migrate two triggers"
        ts =
          [ tableBankSchema1
              { tblVersion = 3
              , tblTriggers = [bankTrigger1, bankTrigger2Proper]
              }
          ]
        ms =
          [ createTriggerMigration 1 bankTrigger1
          , createTriggerMigration 2 bankTrigger2Proper
          ]
    triggerStep msg $ do
      assertNoException msg $ migrate ts ms
      verify [bankTrigger1, bankTrigger2Proper] True

  do
    let msg = "database exception is raised if trigger's WHEN is syntactically incorrect"
        trg = bankTrigger1 {triggerWhen = Just "WILL FAIL"}
        ts =
          [ tableBankSchema1
              { tblVersion = 2
              , tblTriggers = [trg]
              }
          ]
        ms = [createTriggerMigration 1 trg]
    triggerStep msg $ do
      assertDBException msg $ migrate ts ms

  do
    let msg = "database exception is raised if trigger's WHEN uses undefined column"
        trg = bankTrigger1 {triggerWhen = Just "NEW.foobar = 1"}
        ts =
          [ tableBankSchema1
              { tblVersion = 2
              , tblTriggers = [trg]
              }
          ]
        ms = [createTriggerMigration 1 trg]
    triggerStep msg $ do
      assertDBException msg $ migrate ts ms

  do
    -- This trigger is valid. However, the WHEN clause specified in triggerWhen is not
    -- what gets returned from the database. The decompiled and normalized WHEN clause
    -- from the database looks like this:
    --   new.name <> 'foobar'::text
    -- We simply assert an exception, which presumably comes from the migration framework,
    -- while it should actually be a deeper check for just the differing WHEN
    -- clauses. On the other hand, it's probably good enough as it is.
    -- See the comment for 'getDBTriggers' in src/Database/PostgreSQL/PQTypes/Model/Trigger.hs.
    let msg = "checkDatabase fails if WHEN clauses from database and code differ"
        trg = bankTrigger1 {triggerWhen = Just "NEW.name != 'foobar'"}
        ts =
          [ tableBankSchema1
              { tblVersion = 2
              , tblTriggers = [trg]
              }
          ]
        ms = [createTriggerMigration 1 trg]
    triggerStep msg $ do
      assertException msg $ migrate ts ms

  do
    let msg = "successfully migrate trigger with valid WHEN"
        trg = bankTrigger1 {triggerWhen = Just "new.name <> 'foobar'::text"}
        ts =
          [ tableBankSchema1
              { tblVersion = 2
              , tblTriggers = [trg]
              }
          ]
        ms = [createTriggerMigration 1 trg]
    triggerStep msg $ do
      assertNoException msg $ migrate ts ms
      verify [trg] True

  do
    let msg = "successfully migrate a constraint trigger that is deferrable"
        trg = bankTrigger1 {triggerKind = TriggerConstraint Deferrable}
        ts =
          [ tableBankSchema1
              { tblVersion = 2
              , tblTriggers = [trg]
              }
          ]
        ms = [createTriggerMigration 1 trg]
    triggerStep msg $ do
      assertNoException msg $ migrate ts ms
      verify [trg] True

  do
    let msg = "successfully migrate a constraint trigger that is deferrable and initially deferred"
        trg = bankTrigger1 {triggerKind = TriggerConstraint DeferrableInitiallyDeferred}
        ts =
          [ tableBankSchema1
              { tblVersion = 2
              , tblTriggers = [trg]
              }
          ]
        ms = [createTriggerMigration 1 trg]
    triggerStep msg $ do
      assertNoException msg $ migrate ts ms
      verify [trg] True

  do
    let msg = "successfully migrate a regular AFTER trigger"
        trg = bankTrigger1 {triggerKind = TriggerRegular After}
        ts =
          [ tableBankSchema1
              { tblVersion = 2
              , tblTriggers = [trg]
              }
          ]
        ms = [createTriggerMigration 1 trg]
    triggerStep msg $ do
      assertNoException msg $ migrate ts ms
      verify [trg] True

  do
    let msg = "successfully migrate a regular BEFORE trigger"
        trg = bankTrigger1 {triggerKind = TriggerRegular Before}
        ts =
          [ tableBankSchema1
              { tblVersion = 2
              , tblTriggers = [trg]
              }
          ]
        ms = [createTriggerMigration 1 trg]
    triggerStep msg $ do
      assertNoException msg $ migrate ts ms
      verify [trg] True

  do
    let msg = "database exception is raised if dropping trigger that does not exist"
        trg = bankTrigger1
        ts =
          [ tableBankSchema1
              { tblVersion = 2
              , tblTriggers = [trg]
              }
          ]
        ms = [dropTriggerMigration 1 trg]
    triggerStep msg $ do
      assertDBException msg $ migrate ts ms

  do
    let msg = "database exception is raised if dropping trigger function of which does not exist"
        trg = bankTrigger2
        ts =
          [ tableBankSchema1
              { tblVersion = 2
              , tblTriggers = [trg]
              }
          ]
        ms = [dropTriggerMigration 1 trg]
    triggerStep msg $ do
      assertDBException msg $ migrate ts ms

  do
    let msg = "successfully drop trigger"
        trg = bankTrigger1
        ts =
          [ tableBankSchema1
              { tblVersion = 3
              , tblTriggers = []
              }
          ]
        ms = [createTriggerMigration 1 trg, dropTriggerMigration 2 trg]
    triggerStep msg $ do
      assertNoException msg $ migrate ts ms
      verify [trg] False

  do
    let msg = "database exception is raised if dropping trigger twice"
        trg = bankTrigger2
        ts =
          [ tableBankSchema1
              { tblVersion = 3
              , tblTriggers = [trg]
              }
          ]
        ms = [dropTriggerMigration 1 trg, dropTriggerMigration 2 trg]
    triggerStep msg $ do
      assertDBException msg $ migrate ts ms

  do
    let msg = "successfully create trigger with multiple events"
        trg = bankTrigger3
        ts =
          [ tableBankSchema1
              { tblVersion = 2
              , tblTriggers = [trg]
              }
          ]
        ms = [createTriggerMigration 1 trg]
    triggerStep msg $ do
      assertNoException msg $ migrate ts ms
      verify [trg] True

  do
    -- Test that we can create custom trigger functions _and_ read them back
    -- for validation.
    let trigger = bankTrigger4
        modTrigger f t = t {triggerFunction = f (triggerFunction trigger)}
        triggerModifiers =
          [ modTrigger (\fn -> fn {fnSecurity = Definer})
          , modTrigger (\fn -> fn {fnSecurity = Invoker})
          , modTrigger (\fn -> fn {fnConfigurationParameters = mempty})
          , modTrigger (\fn -> fn {fnConfigurationParameters = M.singleton "search_path" "pg_catalog"})
          ]
    forM_ (zip [1 :: Int ..] (map (\f -> f trigger) triggerModifiers)) $ \(i, trg) -> do
      let i' = show i
          idSQL = rawSQL (T.pack (show i)) ()
          msg = "successfully create trigger with custom functions: " <> i'
          trg' =
            trg
              { triggerName = triggerName trg <> idSQL
              , triggerFunction = (triggerFunction trg) {fnName = fnName (triggerFunction trg) <> idSQL}
              }
          ts =
            [ tableBankSchema1
                { tblVersion = 2
                , tblTriggers = [trg']
                }
            ]
          ms = [createTriggerMigration 1 trg']
      triggerStep msg $ do
        assertNoException msg $ migrate ts ms
        verify [trg'] True
  where
    triggerStep msg rest = do
      recreateTriggerDB
      step msg
      rest

    migrate tables migrations = do
      let definitions = tableDefsWithPgCrypto tables
      migrateDatabase testExtrasOptions definitions migrations
      checkDatabase testExtrasOptions definitions

    -- Verify that the given triggers are (not) present in the database.
    verify :: (MonadIO m, MonadDB m, HasCallStack) => [Trigger] -> Bool -> m ()
    verify triggers present = do
      dbTriggers <- getDBTriggers "bank"
      let trgs = dbTriggers
          ok = all (`elem` trgs) triggers
          err = "Triggers " <> (if present then "" else "not ") <> "present in the database."
          trans = if present then id else not
      liftIO . assertBool err $ trans ok

    triggerMigration :: MonadDB m => (Trigger -> m ()) -> Int -> Trigger -> Migration m
    triggerMigration fn from trg =
      Migration
        { mgrTableName = tblName tableBankSchema1
        , mgrFrom = fromIntegral from
        , mgrAction = StandardMigration $ fn trg
        }

    createTriggerMigration :: MonadDB m => Int -> Trigger -> Migration m
    createTriggerMigration = triggerMigration createTrigger

    dropTriggerMigration :: MonadDB m => Int -> Trigger -> Migration m
    dropTriggerMigration = triggerMigration dropTrigger

    recreateTriggerDB = do
      runSQL_ "DROP TRIGGER IF EXISTS trg__bank__trigger_1 ON bank;"
      runSQL_ "DROP TRIGGER IF EXISTS trg__bank__trigger_2 ON bank;"
      runSQL_ "DROP FUNCTION IF EXISTS trgfun__trigger_1;"
      runSQL_ "DROP FUNCTION IF EXISTS trgfun__trigger_2;"
      runSQL_ "DROP TABLE IF EXISTS bank;"
      runSQL_ "DELETE FROM table_versions WHERE name = 'bank'"
      migrate [tableBankSchema1] [createTableMigration tableBankSchema1]

testSqlWith :: HasCallStack => (String -> TestM ()) -> TestM ()
testSqlWith step = do
  step "Running sql WITH tests"
  testPass
  where
    migrate tables migrations = do
      let definitions = tableDefsWithPgCrypto tables
      migrateDatabase testExtrasOptions definitions migrations
      checkDatabase testExtrasOptions definitions
    testPass = do
      step "create the initial database"
      migrate [tableBankSchema1] [createTableMigration tableBankSchema1]
      step "inserting initial data"
      runQuery_ . sqlInsert "bank" $ do
        sqlSetList "name" ["HSBC" :: T.Text, "other"]
        sqlSetList "location" ["13 Foo St., Tucson" :: T.Text, "no address"]
        sqlResult "id"
      step "testing WITH .. INSERT SELECT"
      runQuery_ . sqlInsertSelect "bank" "bank_name" $ do
        sqlWith "bank_name" $ do
          sqlSelect "bank" $ do
            sqlResult "'another'"
            sqlLimit (1 :: Int)
        sqlFrom "bank_name"
        sqlSetCmd "name" "bank_name"
        sqlSet "location" ("Other side" :: T.Text)
      step "testing WITH MATERIALIZED .. UPDATE"
      runQuery_ . sqlUpdate "bank" $ do
        sqlWithMaterialized "other_bank" $ do
          sqlSelect "bank" $ do
            sqlWhereEq "name" ("other" :: T.Text)
            sqlResult "id"
        sqlFrom "other_bank"
        sqlSet "location" ("abcd" :: T.Text)
        sqlWhereInSql "bank.id" $ mkSQL "other_bank.id"
        sqlResult "bank.id"
      step "testing WITH NOT MATERIALIZED .. DELETE"
      runQuery_ . sqlDelete "bank" $ do
        sqlWithNotMaterialized "other_bank" $ do
          sqlSelect "bank" $ do
            sqlWhereEq "name" ("other" :: T.Text)
            sqlResult "id"
        sqlFrom "other_bank"
        sqlWhereInSql "bank.id" $ mkSQL "other_bank.id"
      step "testing WITH .. SELECT"
      runQuery_ . sqlSelect "bank" $ do
        sqlWith "other_bank" $ do
          sqlSelect "bank" $ do
            sqlResult "name"
        sqlFrom "other_bank"
        sqlResult "other_bank.name"
      (results :: [T.Text]) <- fetchMany runIdentity
      liftIO $ assertEqual "Wrong number of banks left" 2 (length results)

testSqlWithRecursive :: HasCallStack => (String -> TestM ()) -> TestM ()
testSqlWithRecursive step = do
  step "Running WITH RECURSIVE tests"
  testPass
  where
    migrate tables migrations = do
      let definitions = tableDefsWithPgCrypto tables
      migrateDatabase testExtrasOptions definitions migrations
      checkDatabase testExtrasOptions definitions
    testPass = do
      step "create the initial database"
      migrate [tableBadGuySchema1, tableCartelSchema1] [createTableMigration tableBadGuySchema1, createTableMigration tableCartelSchema1]
      step "inserting initial data"
      runQuery_ . sqlInsert "bad_guy" $ do
        sqlSetList "firstname" ["Pablo" :: T.Text, "Gustavo", "Mario"]
        sqlSetList "lastname" ["Escobar" :: T.Text, "Rivero", "Vallejo"]
        sqlResult "id"
      (badGuyIds :: [UUID]) <- fetchMany runIdentity
      -- Populate the 'cartel' table
      -- We will have a simple direct hierarchy just to test the recursion:
      -- Pablo is the boss of Gustavo, who is the boss of Mario
      runQuery_ . sqlInsert "cartel" $ do
        sqlSetList "cartel_member_id" badGuyIds
        sqlSetList "cartel_boss_id" $ Nothing : (Just <$> take 2 badGuyIds)
      step "Checking a recursive query on the cartel table"
      runQuery_ . sqlSelect "rcartel" $ do
        sqlWithRecursive "rcartel" $ do
          sqlSelect "cartel root" $ do
            sqlResult "root.cartel_member_id"
            sqlResult "root.cartel_boss_id"
            sqlWhere "root.cartel_boss_id IS NULL"
            sqlUnionAll
              [ sqlSelect "cartel child" $ do
                  sqlResult "child.cartel_member_id"
                  sqlResult "child.cartel_boss_id"
                  sqlJoinOn "rcartel rcartel1" "child.cartel_boss_id = rcartel1.cartel_member_id"
              ]
        -- This dummy with is not actually used
        -- It's just here to test that further "sqlWith" do not remove the RECURSIVE
        -- keyword when actually producing the SQL
        sqlWith "lcartel" $ do
          sqlSelect "rcartel c" $ do
            sqlResult "c.cartel_member_id"
            sqlResult "c.cartel_boss_id"
        sqlResult "member.firstname"
        sqlResult "member.lastname"
        sqlResult "boss.firstname"
        sqlResult "boss.lastname"
        sqlJoinOn "bad_guy member" "rcartel.cartel_member_id = member.id"
        sqlLeftJoinOn "bad_guy boss" "rcartel.cartel_boss_id = boss.id"
      let toCartel :: (T.Text, T.Text, Maybe T.Text, Maybe T.Text) -> (T.Text, Maybe T.Text)
          toCartel (memberFn, memberLn, bossFn, bossLn) =
            (T.intercalate " " [memberFn, memberLn], T.intercalate " " <$> sequence [bossFn, bossLn])
      results <- fetchMany toCartel
      liftIO $
        assertEqual
          "Wrong cartel hierarchy retrieved"
          results
          [ ("Pablo Escobar", Nothing)
          , ("Gustavo Rivero", Just "Pablo Escobar")
          , ("Mario Vallejo", Just "Gustavo Rivero")
          ]

testUnion :: HasCallStack => (String -> TestM ()) -> TestM ()
testUnion step = do
  step "Running SQL UNION tests"
  testPass
  where
    testPass = do
      runQuery_ . sqlSelect "" $ do
        sqlResult "true"
        sqlUnion
          [ sqlSelect "" $ do
              sqlResult "false"
          , sqlSelect "" $ do
              sqlResult "true"
          ]
      result <- fetchMany runIdentity
      liftIO $
        assertEqual
          "UNION of booleans"
          [False, True]
          result

testUnionAll :: HasCallStack => (String -> TestM ()) -> TestM ()
testUnionAll step = do
  step "Running SQL UNION ALL tests"
  testPass
  where
    testPass = do
      runQuery_ . sqlSelect "" $ do
        sqlResult "true"
        sqlUnionAll
          [ sqlSelect "" $ do
              sqlResult "false"
          , sqlSelect "" $ do
              sqlResult "true"
          ]
      result <- fetchMany runIdentity
      liftIO $
        assertEqual
          "UNION ALL of booleans"
          [True, False, True]
          result

migrationTest1 :: ConnectionSourceM (LogT IO) -> TestTree
migrationTest1 connSource =
  testCaseSteps' "Migration test 1" connSource $ \step -> do
    freshTestDB step

    migrationTest1Body step

-- | Test for behaviour of 'checkDatabase' and 'checkDatabaseAllowUnknownObjects'
migrationTest2 :: ConnectionSourceM (LogT IO) -> TestTree
migrationTest2 connSource =
  testCaseSteps' "Migration test 2" connSource $ \step -> do
    freshTestDB step

    createTablesSchema1 step

    let composite =
          CompositeType
            { ctName = "composite"
            , ctColumns =
                [ CompositeColumn {ccName = "cint", ccType = UuidT}
                , CompositeColumn {ccName = "ctext", ccType = TextT}
                ]
            }
        currentSchema = schema1Tables
        differentSchema = schema5Tables
        extrasOptions = testExtrasOptions {eoEnforcePKs = True}
        extrasOptionsWithUnknownObjects = extrasOptions {eoObjectsValidationMode = AllowUnknownObjects}

    runQuery_ $ sqlCreateComposite composite

    assertNoException "checkDatabase should run fine for consistent DB"
      $ checkDatabase extrasOptions
      $ emptyDbDefinitions {dbComposites = [composite], dbTables = currentSchema}
    assertException "checkDatabase fails if composite type definition is not provided"
      $ checkDatabase extrasOptions
      $ emptyDbDefinitions {dbTables = currentSchema}
    assertNoException
      "checkDatabaseAllowUnknownTables runs fine \
      \for consistent DB"
      $ checkDatabase extrasOptionsWithUnknownObjects
      $ emptyDbDefinitions {dbComposites = [composite], dbTables = currentSchema}
    assertNoException
      "checkDatabaseAllowUnknownTables runs fine \
      \for consistent DB with unknown composite type in the database"
      $ checkDatabase extrasOptionsWithUnknownObjects
      $ emptyDbDefinitions {dbTables = currentSchema}
    assertException "checkDatabase should throw exception for wrong schema" $
      checkDatabase extrasOptions emptyDbDefinitions {dbTables = differentSchema}
    assertException
      "checkDatabaseAllowUnknownObjects \
      \should throw exception for wrong scheme"
      $ checkDatabase extrasOptionsWithUnknownObjects emptyDbDefinitions {dbTables = differentSchema}

    runSQL_
      "INSERT INTO table_versions (name, version) \
      \VALUES ('unknown_table', 0)"
    assertException "checkDatabase throw when extra entry in 'table_versions'"
      $ checkDatabase extrasOptions
      $ emptyDbDefinitions {dbTables = currentSchema}
    assertNoException
      "checkDatabaseAllowUnknownObjects \
      \accepts extra entry in 'table_versions'"
      $ checkDatabase extrasOptionsWithUnknownObjects
      $ emptyDbDefinitions {dbTables = currentSchema}
    runSQL_ "DELETE FROM table_versions where name='unknown_table'"

    runSQL_ "CREATE TABLE unknown_table (title text)"
    assertException "checkDatabase should throw with unknown table"
      $ checkDatabase extrasOptions
      $ emptyDbDefinitions {dbTables = currentSchema}
    assertNoException "checkDatabaseAllowUnknownObjects accepts unknown table"
      $ checkDatabase extrasOptionsWithUnknownObjects
      $ emptyDbDefinitions {dbTables = currentSchema}

    runSQL_
      "INSERT INTO table_versions (name, version) \
      \VALUES ('unknown_table', 0)"
    assertException "checkDatabase should throw with unknown table"
      $ checkDatabase extrasOptions
      $ emptyDbDefinitions {dbTables = currentSchema}
    assertNoException
      "checkDatabaseAllowUnknownObjects \
      \accepts unknown tables with version"
      $ checkDatabase extrasOptionsWithUnknownObjects
      $ emptyDbDefinitions {dbTables = currentSchema}

    freshTestDB step

    let withMissingPKSchema = schema6Tables
        schema1MigrationsWithMissingPK = schema6Migrations
        optionsNoPKCheck =
          testExtrasOptions
            { eoEnforcePKs = False
            }
        optionsWithPKCheck =
          testExtrasOptions
            { eoEnforcePKs = True
            }

    step "Recreating the database (schema version 1, one table is missing PK)..."

    migrateDatabase
      optionsNoPKCheck
      (tableDefsWithPgCrypto withMissingPKSchema)
      [schema1MigrationsWithMissingPK]
    checkDatabase optionsNoPKCheck (tableDefsWithPgCrypto withMissingPKSchema)

    assertException
      "checkDatabase should throw when PK missing from table \
      \'participated_in_robbery' and check is enabled"
      $ checkDatabase optionsWithPKCheck
      $ emptyDbDefinitions {dbTables = withMissingPKSchema}
    assertNoException
      "checkDatabase should not throw when PK missing from table \
      \'participated_in_robbery' and check is disabled"
      $ checkDatabase optionsNoPKCheck
      $ emptyDbDefinitions {dbTables = withMissingPKSchema}

    freshTestDB step

migrationTest3 :: ConnectionSourceM (LogT IO) -> TestTree
migrationTest3 connSource =
  testCaseSteps' "Migration test 3" connSource $ \step -> do
    freshTestDB step

    createTablesSchema1 step
    (badGuyIds, robberyIds) <-
      testDBSchema1 step

    migrateDBToSchema2 step
    testDBSchema2 step badGuyIds robberyIds

    assertException
      "Trying to run the same migration twice should fail, \
      \when starting with a createTable migration"
      $ migrateDBToSchema2Hacky step

    freshTestDB step

-- | Test that running the same migrations twice doesn't result in
-- unexpected errors.
migrationTest4 :: ConnectionSourceM (LogT IO) -> TestTree
migrationTest4 connSource =
  testCaseSteps' "Migration test 4" connSource $ \step -> do
    freshTestDB step

    migrationTest1Body step

    -- Here we run step 5 for the second time. This should be a no-op.
    migrateDBToSchema5 step
    testDBSchema5 step

    freshTestDB step

-- | Test triggers.
triggerTests :: ConnectionSourceM (LogT IO) -> TestTree
triggerTests connSource =
  testCaseSteps' "Trigger tests" connSource $ \step -> do
    freshTestDB step
    testTriggers step

sqlWithTests :: ConnectionSourceM (LogT IO) -> TestTree
sqlWithTests connSource =
  testCaseSteps' "sql WITH tests" connSource $ \step -> do
    freshTestDB step
    testSqlWith step

unionTests :: ConnectionSourceM (LogT IO) -> TestTree
unionTests connSource =
  testCaseSteps' "SQL UNION Tests" connSource $ \step -> do
    freshTestDB step
    testUnion step

sqlWithRecursiveTests :: ConnectionSourceM (LogT IO) -> TestTree
sqlWithRecursiveTests connSource =
  testCaseSteps' "SQL WITH RECURSIVE Tests" connSource $ \step -> do
    freshTestDB step
    testSqlWithRecursive step

unionAllTests :: ConnectionSourceM (LogT IO) -> TestTree
unionAllTests connSource =
  testCaseSteps' "SQL UNION ALL Tests" connSource $ \step -> do
    freshTestDB step
    testUnionAll step

eitherExc :: MonadCatch m => (SomeException -> m ()) -> (a -> m ()) -> m a -> m ()
eitherExc left right c = try c >>= either left right

migrationTest5 :: ConnectionSourceM (LogT IO) -> TestTree
migrationTest5 connSource =
  testCaseSteps' "Migration test 5" connSource $ \step -> do
    freshTestDB step

    step "Creating the database (schema version 1)..."
    migrateDatabase testExtrasOptions (tableDefsWithPgCrypto [table1]) [createTableMigration table1]
    checkDatabase testExtrasOptions (tableDefsWithPgCrypto [table1])

    step "Populating the 'bank' table..."
    runQuery_ . sqlInsert "bank" $ do
      sqlSetList "name" $ (\i -> "bank" <> show i) <$> numbers
      sqlSetList "location" $ (\i -> "location" <> show i) <$> numbers

    -- Explicitly vacuum to update the catalog so that getting the row number
    -- estimates works. Note that vacuum can't run inside a transaction block,
    -- which every test runs in.
    unsafeWithoutTransaction $ runSQL_ "VACUUM bank"

    forM_ (zip4 tables migrations steps assertions) $
      \(table, migration, step', assertion) -> do
        step step'
        migrateDatabase testExtrasOptions (tableDefsWithPgCrypto [table]) [migration]
        checkDatabase testExtrasOptions (tableDefsWithPgCrypto [table])
        uncurry assertNoException assertion

    freshTestDB step
  where
    -- Chosen by a fair dice roll.
    numbers = [1 .. 101] :: [Int]
    table1 = tableBankSchema1
    tables =
      [ table1
          { tblVersion = 2
          , tblColumns = tblColumns table1 ++ [stringColumn]
          }
      , table1
          { tblVersion = 3
          , tblColumns = tblColumns table1 ++ [stringColumn]
          }
      , table1
          { tblVersion = 4
          , tblColumns = tblColumns table1 ++ [stringColumn, boolColumn]
          }
      , table1
          { tblVersion = 5
          , tblColumns = tblColumns table1 ++ [stringColumn, boolColumn]
          }
      ]

    migrations =
      [ addStringColumnMigration
      , copyStringColumnMigration
      , addBoolColumnMigration
      , modifyBoolColumnMigration
      ]

    steps =
      [ "Adding string column (version 1 -> version 2)..."
      , "Copying string column (version 2 -> version 3)..."
      , "Adding bool column (version 3 -> version 4)..."
      , "Modifying bool column (version 4 -> version 5)..."
      ]

    assertions =
      [ ("Check that the string column has been added" :: String, checkAddStringColumn)
      , ("Check that the string data has been copied", checkCopyStringColumn)
      , ("Check that the bool column has been added", checkAddBoolColumn)
      , ("Check that the bool column has been modified", checkModifyBoolColumn)
      ]

    stringColumn =
      tblColumn
        { colName = "name_new"
        , colType = TextT
        }

    boolColumn =
      tblColumn
        { colName = "name_is_true"
        , colType = BoolT
        , colNullable = False
        , colDefault = Just "false"
        }

    cursorSql = "SELECT id FROM bank" :: SQL

    addStringColumnMigration =
      Migration
        { mgrTableName = "bank"
        , mgrFrom = 1
        , mgrAction =
            StandardMigration
              $ runQuery_
              $ sqlAlterTable "bank" [sqlAddColumn stringColumn]
        }

    copyStringColumnMigration =
      Migration
        { mgrTableName = "bank"
        , mgrFrom = 2
        , mgrAction = ModifyColumnMigration cursorSql copyColumnSql 1000
        }
    copyColumnSql :: MonadDB m => [Identity UUID] -> m ()
    copyColumnSql primaryKeys =
      runQuery_ . sqlUpdate "bank" $ do
        sqlSetCmd "name_new" "bank.name"
        sqlWhereEqualsAny "bank.id" $ runIdentity <$> primaryKeys

    addBoolColumnMigration =
      Migration
        { mgrTableName = "bank"
        , mgrFrom = 3
        , mgrAction =
            StandardMigration
              $ runQuery_
              $ sqlAlterTable "bank" [sqlAddColumn boolColumn]
        }

    modifyBoolColumnMigration =
      Migration
        { mgrTableName = "bank"
        , mgrFrom = 4
        , mgrAction = ModifyColumnMigration cursorSql modifyColumnSql 1000
        }
    modifyColumnSql :: MonadDB m => [Identity UUID] -> m ()
    modifyColumnSql primaryKeys =
      runQuery_ . sqlUpdate "bank" $ do
        sqlSet "name_is_true" True
        sqlWhereIn "bank.id" $ runIdentity <$> primaryKeys

    checkAddStringColumn = do
      runQuery_ . sqlSelect "bank" $ sqlResult "name_new"
      rows :: [Maybe T.Text] <- fetchMany runIdentity
      liftIO . assertEqual "No name_new in empty column" True $ all (== Nothing) rows

    checkCopyStringColumn = do
      runQuery_ . sqlSelect "bank" $ sqlResult "name_new"
      rows_new :: [Maybe T.Text] <- fetchMany runIdentity
      runQuery_ . sqlSelect "bank" $ sqlResult "name"
      rows_old :: [Maybe T.Text] <- fetchMany runIdentity
      liftIO . assertEqual "All name_new are equal name" True
        $ all (uncurry (==))
        $ zip rows_new rows_old

    checkAddBoolColumn = do
      runQuery_ . sqlSelect "bank" $ sqlResult "name_is_true"
      rows :: [Maybe Bool] <- fetchMany runIdentity
      liftIO . assertEqual "All name_is_true default to false" True $ all (== Just False) rows

    checkModifyBoolColumn = do
      runQuery_ . sqlSelect "bank" $ sqlResult "name_is_true"
      rows :: [Maybe Bool] <- fetchMany runIdentity
      liftIO . assertEqual "All name_is_true are true" True $ all (== Just True) rows

foreignKeyIndexesTests :: ConnectionSourceM (LogT IO) -> TestTree
foreignKeyIndexesTests connSource =
  testCaseSteps' "Foreign key indexes tests" connSource $ \step -> do
    freshTestDB step

    step "Create database with two tables, no foreign key checking"
    do
      let options = testExtrasOptions
      migrateDatabase
        options
        (tableDefsWithPgCrypto [table1, table2])
        [createTableMigration table1, createTableMigration table2]
      checkDatabase testExtrasOptions (tableDefsWithPgCrypto [table1, table2])

    step "Create database with two tables, with foreign key checking"
    do
      let options = testExtrasOptions {eoCheckForeignKeysIndexes = True}
      assertException "Foreign keys are missing" $
        migrateDatabase
          options
          (tableDefsWithPgCrypto [table1, table2])
          [createTableMigration table1, createTableMigration table2]

    step "Table is missing several foreign key indexes"
    do
      let options = testExtrasOptions {eoCheckForeignKeysIndexes = True}
      assertException "Foreign keys are missing" $
        migrateDatabase
          options
          (tableDefsWithPgCrypto [table1, table2, table3])
          [createTableMigration table1, createTableMigration table2, createTableMigration table3]

    step "Multi column indexes covering a FK pass the checks"
    do
      let options = testExtrasOptions {eoCheckForeignKeysIndexes = True}
      migrateDatabase
        options
        (tableDefsWithPgCrypto [table4])
        [ dropTableMigration table1
        , dropTableMigration table2
        , dropTableMigration table3
        , createTableMigration table4
        ]
      checkDatabase options (tableDefsWithPgCrypto [table4])
    step "Multi column indexes not covering a FK fail the checks"
    do
      let options = testExtrasOptions {eoCheckForeignKeysIndexes = True}
      assertException "Foreign keys are missing" $
        migrateDatabase
          options
          (tableDefsWithPgCrypto [table5])
          [ dropTableMigration table4
          , createTableMigration table5
          ]
  where
    table1 :: Table
    table1 =
      tblTable
        { tblName = "fktest1"
        , tblVersion = 1
        , tblColumns =
            [ tblColumn {colName = "id", colType = UuidT, colNullable = False}
            , tblColumn {colName = "name", colType = TextT}
            , tblColumn {colName = "location", colType = TextT}
            ]
        , tblPrimaryKey = pkOnColumn "id"
        }
    table2 :: Table
    table2 =
      tblTable
        { tblName = "fktest2"
        , tblVersion = 1
        , tblColumns =
            [ tblColumn {colName = "id", colType = UuidT, colNullable = False}
            , tblColumn {colName = "fkid", colType = UuidT}
            , tblColumn {colName = "fkname", colType = TextT}
            ]
        , tblPrimaryKey = pkOnColumn "id"
        , tblForeignKeys =
            [ fkOnColumn "fkid" "fktest1" "id"
            ]
        }
    table3 :: Table
    table3 =
      tblTable
        { tblName = "fktest3"
        , tblVersion = 1
        , tblColumns =
            [ tblColumn {colName = "id", colType = UuidT, colNullable = False}
            , tblColumn {colName = "fk1id", colType = UuidT}
            , tblColumn {colName = "fk2id", colType = UuidT}
            , tblColumn {colName = "fkname", colType = TextT}
            ]
        , tblPrimaryKey = pkOnColumn "id"
        , tblForeignKeys =
            [ fkOnColumn "fk1id" "fktest1" "id"
            , fkOnColumn "fk2id" "fktest2" "id"
            ]
        }
    table4 :: Table
    table4 =
      tblTable
        { tblName = "fktest4"
        , tblVersion = 1
        , tblColumns =
            [ tblColumn {colName = "id", colType = UuidT, colNullable = False}
            , tblColumn {colName = "fk4id", colType = UuidT}
            , tblColumn {colName = "fk4name", colType = TextT}
            ]
        , tblPrimaryKey = pkOnColumn "id"
        , tblForeignKeys =
            [ fkOnColumn "fk4id" "fktest4" "id"
            ]
        , tblIndexes =
            [ indexOnColumns [indexColumn "fk4id", indexColumn "fk4name"]
            ]
        }
    table5 :: Table
    table5 =
      tblTable
        { tblName = "fktest5"
        , tblVersion = 1
        , tblColumns =
            [ tblColumn {colName = "id", colType = UuidT, colNullable = False}
            , tblColumn {colName = "fk5id", colType = UuidT}
            , tblColumn {colName = "fk5name", colType = TextT}
            ]
        , tblPrimaryKey = pkOnColumn "id"
        , tblForeignKeys =
            [ fkOnColumn "fk5id" "fktest5" "id"
            ]
        , tblIndexes =
            [ indexOnColumns [indexColumn "fk5thing", indexColumn "fk5id"]
            ]
        }

overlapingIndexesTests :: ConnectionSourceM (LogT IO) -> TestTree
overlapingIndexesTests connSource = do
  testCaseSteps' "Overlapping indexes tests" connSource $ \step -> do
    freshTestDB step

    let definitions = tableDefsWithPgCrypto [table1]

    step "Migration is correct if not checking for overlapping indexes"
    do
      let options = testExtrasOptions {eoCheckOverlappingIndexes = False}
      migrateDatabase
        options
        definitions
        [createTableMigration table1]

    step "Migration invalid when flagging overlapping indexes"
    do
      let options = testExtrasOptions {eoCheckOverlappingIndexes = True}
      assertException "Some indexes are overlapping" $
        migrateDatabase
          options
          definitions
          [createTableMigration table1]
  where
    table1 :: Table
    table1 =
      tblTable
        { tblName = "idx_test"
        , tblVersion = 1
        , tblColumns =
            [ tblColumn {colName = "id", colType = UuidT, colNullable = False}
            , tblColumn {colName = "idx1", colType = UuidT}
            , tblColumn {colName = "idx2", colType = UuidT}
            , tblColumn {colName = "idx3", colType = UuidT}
            ]
        , tblPrimaryKey = pkOnColumn "id"
        , tblIndexes =
            [ indexOnColumns [indexColumn "idx1", indexColumn "idx2"]
            , indexOnColumns [indexColumn "idx1"]
            ]
        }

nullsNotDistinctTests :: ConnectionSourceM (LogT IO) -> TestTree
nullsNotDistinctTests connSource = do
  testCaseSteps' "NULLS NOT DISTINCT tests" connSource $ \step -> do
    freshTestDB step

    step "Create a database with indexes"
    do
      let definitions = tableDefsWithPgCrypto [nullTableTest1, nullTableTest2]
      migrateDatabase
        testExtrasOptions
        definitions
        [createTableMigration nullTableTest1, createTableMigration nullTableTest2]
      checkDatabase testExtrasOptions definitions

    step "Insert two NULLs on a column with a default UNIQUE index"
    do
      runQuery_ . sqlInsert "nulltests1" $ do
        sqlSet "content" (Nothing @T.Text)
      runQuery_ . sqlInsert "nulltests1" $ do
        sqlSet "content" (Nothing @T.Text)

    step "Insert NULLs on a column with a NULLS NOT DISTINCT index"
    do
      runQuery_ . sqlInsert "nulltests2" $ do
        sqlSet "content" (Nothing @T.Text)
      assertDBException "Cannot insert twice a null value with NULLS NOT DISTINCT" $ runQuery_ . sqlInsert "nulltests2" $ do
        sqlSet "content" (Nothing @T.Text)
  where
    nullTableTest1 =
      tblTable
        { tblName = "nulltests1"
        , tblVersion = 1
        , tblColumns =
            [ tblColumn {colName = "id", colType = UuidT, colNullable = False, colDefault = Just "gen_random_uuid()"}
            , tblColumn {colName = "content", colType = TextT, colNullable = True}
            ]
        , tblPrimaryKey = pkOnColumn "id"
        , tblIndexes =
            [ uniqueIndexOnColumn "content"
            ]
        }

    nullTableTest2 =
      tblTable
        { tblName = "nulltests2"
        , tblVersion = 1
        , tblColumns =
            [ tblColumn {colName = "id", colType = UuidT, colNullable = False, colDefault = Just "gen_random_uuid()"}
            , tblColumn {colName = "content", colType = TextT, colNullable = True}
            ]
        , tblPrimaryKey = pkOnColumn "id"
        , tblIndexes =
            [ (uniqueIndexOnColumn "content") {idxNotDistinctNulls = True}
            ]
        }

sqlAnyAllTests :: TestTree
sqlAnyAllTests =
  testGroup
    "SQL ANY/ALL tests"
    [ testCase "sqlAll produces correct queries" $ do
        assertSqlEqual "empty sqlAll is TRUE" "TRUE" . sqlAll $ pure ()
        assertSqlEqual "sigle condition is emmited as is" "cond" $ sqlAll $ sqlWhere "cond"
        assertSqlEqual
          "each condition as well as entire condition is parenthesized"
          "((cond1) AND (cond2))"
          $ sqlAll
          $ do
            sqlWhere "cond1"
            sqlWhere "cond2"

        assertSqlEqual
          "sqlAll can be nested"
          "((cond1) AND (cond2) AND (((cond3) AND (cond4))))"
          $ sqlAll
          $ do
            sqlWhere "cond1"
            sqlWhere "cond2"
            sqlWhere . sqlAll $ do
              sqlWhere "cond3"
              sqlWhere "cond4"
    , testCase "sqlAny produces correct queries" $ do
        assertSqlEqual "empty sqlAny is FALSE" "FALSE" . sqlAny $ pure ()
        assertSqlEqual "sigle condition is emmited as is" "cond" $ sqlAny $ sqlWhere "cond"
        assertSqlEqual
          "each condition as well as entire condition is parenthesized"
          "((cond1) OR (cond2))"
          $ sqlAny
          $ do
            sqlWhere "cond1"
            sqlWhere "cond2"

        assertSqlEqual
          "sqlAny can be nested"
          "((cond1) OR (cond2) OR (((cond3) OR (cond4))))"
          $ sqlAny
          $ do
            sqlWhere "cond1"
            sqlWhere "cond2"
            sqlWhere . sqlAny $ do
              sqlWhere "cond3"
              sqlWhere "cond4"
    , testCase "mixing sqlAny and all produces correct queries" $ do
        assertSqlEqual
          "sqlAny and sqlAll can be mixed"
          "((((cond1) OR (cond2))) AND (((cond3) OR (cond4))))"
          $ sqlAll
          $ do
            sqlWhere . sqlAny $ do
              sqlWhere "cond1"
              sqlWhere "cond2"
            sqlWhere . sqlAny $ do
              sqlWhere "cond3"
              sqlWhere "cond4"
    , testCase "sqlWhereAny produces correct queries" $ do
        -- `sqlWhereAny` has to be wrapped in `sqlAll` to disambiguate the `SqlWhere` monad.
        assertSqlEqual "empty sqlWhereAny is FALSE" "FALSE" . sqlAll $ sqlWhereAny []
        assertSqlEqual
          "each condition as well as entire condition is parenthesized and joined with OR"
          "((cond1) OR (cond2))"
          . sqlAll
          $ sqlWhereAny [sqlWhere "cond1", sqlWhere "cond2"]
        assertSqlEqual
          "nested multi-conditions are parenthesized and joined with AND"
          "((cond1) OR (((cond2) AND (cond3))) OR (cond4))"
          . sqlAll
          $ sqlWhereAny
            [ sqlWhere "cond1"
            , do
                sqlWhere "cond2"
                sqlWhere "cond3"
            , sqlWhere "cond4"
            ]
    ]
  where
    assertSqlEqual :: Sqlable a => String -> a -> a -> Assertion
    assertSqlEqual msg a b =
      assertEqual
        msg
        (show $ toSQLCommand a)
        (show $ toSQLCommand b)

enumTest :: ConnectionSourceM (LogT IO) -> TestTree
enumTest connSource =
  testCaseSteps' "Enum tests" connSource $ \step -> do
    freshTestDB step

    step "Create a database with an enum"
    migrateDatabase
      testExtrasOptions
      (emptyDbDefinitions {dbEnums = [enum1]})
      []

    step "Check the database"
    checkDatabase testExtrasOptions (emptyDbDefinitions {dbEnums = [enum1]})

    step "Check the database with missing enum"
    do
      report <- checkDatabaseWithReport testExtrasOptions (emptyDbDefinitions {dbEnums = [enum1, enum2]})
      liftIO $
        assertEqual
          "Missing enum2 should be reported"
          (validationError "Enum 'enum2' doesn't exist in the database")
          report

    step "Check the database with reordered enum"
    do
      report <- checkDatabaseWithReport testExtrasOptions (emptyDbDefinitions {dbEnums = [enum1misorder]})
      liftIO $
        assertEqual
          "Order mismatch should be reported"
          ( validationInfo
              "Enum 'enum1' has same values, but differs in order \
              \(database: [\"enum-100\",\"enum-101\"], definition: [\"enum-101\",\"enum-100\"]). \
              \This isn't usually a problem, unless the enum is used for ordering."
          )
          report

    step "Check the database with mismatching enum"
    do
      report <- checkDatabaseWithReport testExtrasOptions (emptyDbDefinitions {dbEnums = [enum1mismatch]})
      liftIO $
        assertEqual
          "DB mismatch should be reported"
          (validationError "Enum 'enum1' does not match (database: [\"enum-100\",\"enum-101\"], definition: [\"enum-100\",\"enum-102\"])")
          report

    step "Check the database with extra enum values"
    do
      report <- checkDatabaseWithReport testExtrasOptions (emptyDbDefinitions {dbEnums = [enum1missing]})
      liftIO $
        assertEqual
          "Extra values in the DB enum should be reported"
          (validationInfo "Enum 'enum1' has all necessary values, but the database has additional ones (database: [\"enum-100\",\"enum-101\"], definition: [\"enum-100\"])")
          report
  where
    enum1 = EnumType {etName = "enum1", etValues = ["enum-100", "enum-101"]}
    enum2 = EnumType {etName = "enum2", etValues = ["enum-200", "enum-201", "enum-202"]}
    enum1misorder = EnumType {etName = "enum1", etValues = ["enum-101", "enum-100"]}
    enum1mismatch = EnumType {etName = "enum1", etValues = ["enum-100", "enum-102"]}
    enum1missing = EnumType {etName = "enum1", etValues = ["enum-100"]}

assertNoException :: String -> TestM () -> TestM ()
assertNoException t =
  eitherExc
    (const $ liftIO $ assertFailure ("Exception thrown for: " ++ t))
    (const $ return ())

assertException :: String -> TestM () -> TestM ()
assertException t =
  eitherExc
    (const $ return ())
    (const $ liftIO $ assertFailure ("No exception thrown for: " ++ t))

assertDBException :: String -> TestM () -> TestM ()
assertDBException t c =
  try c
    >>= either
      (\DBException {} -> pure ())
      (const . liftIO . assertFailure $ "No DBException thrown for: " ++ t)

-- | A variant of testCaseSteps that works in TestM monad.
testCaseSteps'
  :: TestName
  -> ConnectionSourceM (LogT IO)
  -> ((String -> TestM ()) -> TestM ())
  -> TestTree
testCaseSteps' testName connSource f =
  testCaseSteps testName $ \step' -> do
    let step s = liftIO $ step' s
    withStdOutLogger $ \logger ->
      runLogT "hpqtypes-extras-test" logger defaultLogLevel
        $ runDBT connSource defaultTransactionSettings
        $ f step

tableDefsWithPgCrypto :: [Table] -> DatabaseDefinitions
tableDefsWithPgCrypto tables =
  emptyDbDefinitions
    { dbTables = tables
    , dbExtensions = ["pgcrypto"]
    , dbDomains = [testDomainColor]
    }
  where
    testDomainColor =
      Domain
        { domName = "test_domain_color"
        , domType = TextT
        , domNullable = False
        , domDefault = Nothing
        , domChecks =
            mkChecks
              [ tblCheck
                  { chkName = "test_domain_color"
                  , chkCondition = "VALUE ~ '^#[0-9a-f]{6}$'::text"
                  }
              ]
        }

numericColumnTypeTestTable :: Table
numericColumnTypeTestTable =
  tblTable
    { tblName = "unicorns"
    , tblVersion = 1
    , tblColumns =
        [ tblColumn
            { colName = "id"
            , colType = UuidT
            , colNullable = False
            , colDefault = Just "gen_random_uuid()"
            }
        , tblColumn
            { colName = "balance"
            , colType = NumericT (Just (1000, 0))
            , colCollation = Nothing
            , colNullable = False
            }
        , tblColumn
            { colName = "employees"
            , colType = NumericT (Just (5, 0))
            , colCollation = Nothing
            , colNullable = False
            }
        , tblColumn
            { colName = "baz"
            , colType = NumericT (Just (5, -32))
            , colCollation = Nothing
            , colNullable = False
            }
        , tblColumn
            { colName = "foo"
            , colType = NumericT (Just (5, 2))
            , colCollation = Nothing
            , colNullable = False
            }
        , tblColumn
            { colName = "bar"
            , colType = NumericT Nothing
            , colCollation = Nothing
            , colNullable = False
            }
        ]
    , tblPrimaryKey = pkOnColumn "id"
    , tblTriggers = []
    }

numericColumnTypeTestMigration :: MonadDB m => Migration m
numericColumnTypeTestMigration =
  let tableName = tblName numericColumnTypeTestTable
  in Migration
       { mgrTableName = tableName
       , mgrFrom = 0
       , mgrAction = StandardMigration $ do
           runQuery_
             $ sqlAlterTable
               tableName
             $ sqlAddColumn <$> tblColumns numericColumnTypeTestTable
       }

numericColumnTypeTest :: ConnectionSourceM (LogT IO) -> TestTree
numericColumnTypeTest connSource =
  testCaseSteps' "NUMERIC column test" connSource $ \step -> do
    freshTestDB step

    let dbDefinitions = emptyDbDefinitions {dbTables = [numericColumnTypeTestTable]}

    step "run the migration"
    migrateDatabase testExtrasOptions dbDefinitions [numericColumnTypeTestMigration]

    step "check database"
    checkDatabase testExtrasOptions dbDefinitions

main :: IO ()
main = do
  (connString, args) <- getConnString
  let connSettings = defaultConnectionSettings {csConnInfo = connString}
      ConnectionSource connSource = simpleSource connSettings
  withArgs args . defaultMain $
    testGroup
      "DB tests"
      [ migrationTest1 connSource
      , migrationTest2 connSource
      , migrationTest3 connSource
      , migrationTest4 connSource
      , migrationTest5 connSource
      , triggerTests connSource
      , sqlWithTests connSource
      , unionTests connSource
      , unionAllTests connSource
      , sqlWithRecursiveTests connSource
      , foreignKeyIndexesTests connSource
      , overlapingIndexesTests connSource
      , nullsNotDistinctTests connSource
      , sqlAnyAllTests
      , enumTest connSource
      , numericColumnTypeTest connSource
      ]
  where
    getConnString :: IO (T.Text, [String])
    getConnString =
      getArgs >>= \case
        connString : args -> pure (T.pack connString, args)
        [] ->
          lookupEnv "GITHUB_ACTIONS" >>= \case
            Just "true" -> pure ("host=localhost user=postgres password=postgres", [])
            _ -> printUsage >> exitFailure

    printUsage :: IO ()
    printUsage = do
      prog <- getProgName
      putStrLn $ "Usage: " <> prog <> " <connection info string> [tasty args]"
