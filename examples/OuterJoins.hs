module OuterJoins (outerJoins) where

import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Data.Int
import Data.Pool
import Data.Text qualified as T
import Database.PostgreSQL.PQTypes
import GHC.Generics

-- | Generic 'putStrLn'.
printLn :: MonadBase IO m => String -> m ()
printLn = liftBase . putStrLn

----------------------------------------

tmpID :: Int64
tmpID = 0

data Attribute = Attribute
  { attrID :: !Int64
  , attrKey :: !T.Text
  , attrValues :: ![T.Text]
  }
  deriving stock (Generic, Show)

data Thing = Thing
  { thingID :: !Int64
  , thingName :: !T.Text
  , thingAttributes :: ![Attribute]
  }
  deriving stock (Show)

instance FromSQL Attribute where
  fromSQL = decodeComposite genericDecoder

withDB :: ConnectionSettings -> IO () -> IO ()
withDB settings = bracket_ createStructure dropStructure
  where
    ConnectionSource cs = simpleSource settings

    createStructure = runDBT cs defaultTransactionSettings $ do
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
    -- Drop previously created database structures.
    dropStructure = runDBT cs defaultTransactionSettings $ do
      printLn "Dropping tables..."
      runSQL_ "DROP TABLE values_"
      runSQL_ "DROP TABLE attributes_"
      runSQL_ "DROP TABLE things_"

insertThings :: [Thing] -> DBT IO ()
insertThings = mapM_ $ \Thing {..} -> do
  runQuery_ $
    rawSQL
      "INSERT INTO things_ (name) VALUES ($1) RETURNING id"
      (Identity thingName)
  tid <- fetchOne $ fromSQL @Int64
  forM_ thingAttributes $ \Attribute {..} -> do
    runQuery_ $
      rawSQL
        "INSERT INTO attributes_ (key, thing_id) VALUES ($1, $2) RETURNING id"
        (attrKey, tid)
    aid <- fetchOne $ fromSQL @Int64
    forM_ attrValues $ \value ->
      runQuery_ $
        rawSQL
          "INSERT INTO values_ (attribute_id, value) VALUES ($1, $2)"
          (aid, value)

selectThings :: DBT IO [Thing]
selectThings = do
  runSQL_ $ "SELECT t.id, t.name, ARRAY(" <> attributes <> ") FROM things_ t ORDER BY t.id"
  fetchMany $
    Thing
      <$> fromSQL
      <*> fromSQL
      <*> fromSQL
  where
    attributes = "SELECT (a.id, a.key, ARRAY(" <> values <> ")) FROM attributes_ a WHERE a.thing_id = t.id ORDER BY a.id"
    values = "SELECT v.value FROM values_ v WHERE v.attribute_id = a.id ORDER BY v.value"

outerJoins :: T.Text -> IO ()
outerJoins connInfo = do
  let cs = defaultConnectionSettings {csConnInfo = connInfo}
  withDB cs $ do
    ConnectionSource pool <- poolSource cs $ \connect disconnect ->
      defaultPoolConfig connect disconnect 1 10
    runDBT pool defaultTransactionSettings $ do
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
