{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module OuterJoins (runApp) where

import Control.Monad
import Data.Int
import Data.Pool
import Data.Text qualified as T
import Effectful
import Effectful.Exception
import Effectful.HPQTypes

-- | Generic 'putStrLn'.
printLn :: IOE :> es => String -> Eff es ()
printLn = liftIO . putStrLn

----------------------------------------

tmpID :: Int64
tmpID = 0

data Attribute = Attribute
  { attrID :: !Int64
  , attrKey :: !String
  , attrValues :: ![String]
  }
  deriving (Show)

data Thing = Thing
  { thingID :: !Int64
  , thingName :: !String
  , thingAttributes :: ![Attribute]
  }
  deriving (Show)

type instance CompositeRow Attribute = (Int64, String, Array1 String)

instance PQFormat Attribute where
  pqFormat = "%attribute_"

instance CompositeFromSQL Attribute where
  toComposite (aid, key, Array1 values) =
    Attribute
      { attrID = aid
      , attrKey = key
      , attrValues = values
      }

withDB :: IOE :> es => ConnectionSettings -> Eff es () -> Eff es ()
withDB cs = bracket_ createStructure dropStructure
  where
    ConnectionSource source = simpleSource cs

    createStructure = runDB source defaultTransactionSettings $ do
      printLn "Creating tables..."
      runSQL_ $
        mconcat
          [ "CREATE TABLE things_ ("
          , "  id BIGSERIAL NOT NULL"
          , ", name TEXT NOT NULL"
          , ", PRIMARY KEY (id)"
          , ")"
          ]
      runSQL_ $
        mconcat
          [ "CREATE TABLE attributes_ ("
          , "  id BIGSERIAL NOT NULL"
          , ", key TEXT NOT NULL"
          , ", thing_id BIGINT NOT NULL"
          , ", PRIMARY KEY (id)"
          , ", FOREIGN KEY (thing_id) REFERENCES things_ (id)"
          , ")"
          ]
      runSQL_ $
        mconcat
          [ "CREATE TABLE values_ ("
          , "  attribute_id BIGINT NOT NULL"
          , ", value TEXT NOT NULL"
          , ", FOREIGN KEY (attribute_id) REFERENCES attributes_ (id)"
          , ")"
          ]
      runSQL_ $
        mconcat
          [ "CREATE TYPE attribute_ AS ("
          , "  id BIGINT"
          , ", key TEXT"
          , ", value TEXT[]"
          , ")"
          ]

    dropStructure = runDB source defaultTransactionSettings $ do
      printLn "Dropping tables..."
      runSQL_ "DROP TYPE attribute_"
      runSQL_ "DROP TABLE values_"
      runSQL_ "DROP TABLE attributes_"
      runSQL_ "DROP TABLE things_"

insertThings :: DB :> es => [Thing] -> Eff es ()
insertThings = mapM_ $ \Thing {..} -> do
  runQuery_ $
    rawSQL
      "INSERT INTO things_ (name) VALUES ($1) RETURNING id"
      (Identity thingName)
  tid <- fetchOne (runIdentity @Int64)
  forM_ thingAttributes $ \Attribute {..} -> do
    runQuery_ $
      rawSQL
        "INSERT INTO attributes_ (key, thing_id) VALUES ($1, $2) RETURNING id"
        (attrKey, tid)
    aid <- fetchOne (runIdentity @Int64)
    forM_ attrValues $ \value ->
      runQuery_ $
        rawSQL
          "INSERT INTO values_ (attribute_id, value) VALUES ($1, $2)"
          (aid, value)

selectThings :: DB :> es => Eff es [Thing]
selectThings = do
  runSQL_ $ "SELECT t.id, t.name, ARRAY(" <> attributes <> ") FROM things_ t ORDER BY t.id"
  fetchMany $ \(tid, name, CompositeArray1 attrs) ->
    Thing
      { thingID = tid
      , thingName = name
      , thingAttributes = attrs
      }
  where
    attributes = "SELECT (a.id, a.key, ARRAY(" <> values <> "))::attribute_ FROM attributes_ a WHERE a.thing_id = t.id ORDER BY a.id"
    values = "SELECT v.value FROM values_ v WHERE v.attribute_id = a.id ORDER BY v.value"

runApp :: T.Text -> IO ()
runApp connInfo = runEff $ do
  let cs = defaultConnectionSettings {csConnInfo = connInfo}
  withDB cs $ do
    ConnectionSource pool <- liftIO $ do
      poolSource
        (cs {csComposites = ["attribute_"]})
        (\create destroy -> defaultPoolConfig create destroy 10 4)
    runDB pool defaultTransactionSettings $ do
      insertThings
        [ Thing
            { thingID = tmpID
            , thingName = "thing1"
            , thingAttributes =
                [ Attribute
                    { attrID = tmpID
                    , attrKey = "key1"
                    , attrValues = ["foo"]
                    }
                , Attribute
                    { attrID = tmpID
                    , attrKey = "key2"
                    , attrValues = []
                    }
                ]
            }
        , Thing
            { thingID = tmpID
            , thingName = "thing2"
            , thingAttributes =
                [ Attribute
                    { attrID = tmpID
                    , attrKey = "key2"
                    , attrValues = ["bar", "baz"]
                    }
                ]
            }
        ]
      selectThings >>= mapM_ (printLn . show)
