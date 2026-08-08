module Main (main) where

import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Data.List qualified as L
import Data.Maybe
import Data.Proxy
import Data.Text qualified as T
import System.Environment
import System.Exit
import System.Random (randomIO)
import Test.QuickCheck.Random
import Test.Tasty
import Test.Tasty.Options

import Data.Monoid.Utils
import Database.PostgreSQL.PQTypes

import Test.Array
import Test.Connection
import Test.Cursor
import Test.Enum
import Test.Env
import Test.RowDecoder
import Test.Transaction
import Test.Tuples
import Test.Types

-- | Seed of the random value generator. Every test starts from the same
-- initial generator derived from it, so a run is reproducible even though
-- tests run in parallel.
newtype Seed = Seed (Maybe Int)

instance IsOption Seed where
  defaultValue = Seed Nothing
  parseValue = fmap (Seed . Just) . safeRead
  optionName = pure "seed"
  optionHelp = pure "Seed of the random value generator (random by default)"

tests :: TestData -> [TestTree]
tests td =
  concat
    [ connectionTests td
    , transactionTests td
    , cursorTests td
    , rowDecoderTests td
    , arrayTests td
    , enumTests td
    , tupleTests td
    , typesTests td
    ]

createStructures :: ConnectionSourceM IO -> IO ()
createStructures cs = runDBT cs defaultTransactionSettings $ do
  liftBase . putStrLn $ "Creating structures..."
  runSQL_ "CREATE TABLE test1_ (a INTEGER)"
  runSQL_ "CREATE TYPE simple_ AS (a INTEGER, b DATE)"
  runSQL_ "CREATE TYPE nested_ AS (d DOUBLE PRECISION, s SIMPLE_)"
  runSQL_ "CREATE TYPE person_ AS ENUM ('alfred', 'bertrand', 'charles')"
  runSQL_ "CREATE TABLE people_ (p PERSON_ NOT NULL)"

dropStructures :: ConnectionSourceM IO -> IO ()
dropStructures cs = runDBT cs defaultTransactionSettings $ do
  liftBase . putStrLn $ "Dropping structures..."
  runSQL_ "DROP TABLE people_"
  runSQL_ "DROP TYPE person_"
  runSQL_ "DROP TYPE nested_"
  runSQL_ "DROP TYPE simple_"
  runSQL_ "DROP TABLE test1_"

getConnString :: IO (T.Text, [String])
getConnString =
  getArgs >>= \case
    connString : args -> pure (T.pack connString, args)
    [] ->
      lookupEnv "GITHUB_ACTIONS" >>= \case
        Just "true" -> pure ("host=postgres user=postgres password=postgres", [])
        _ -> printUsage >> exitFailure
  where
    printUsage = do
      prog <- getProgName
      putStrLn $
        "Usage:"
          <+> prog
          <+> "<connection info string> [test-framework args]"

main :: IO ()
main = do
  (connString, args) <- getConnString
  let connSettings =
        defaultConnectionSettings
          { csConnInfo = connString
          , csClientEncoding = Just "latin1"
          }
      ConnectionSource connSource = simpleSource connSettings

  createStructures connSource
  defaultSeed <- randomIO

  let allTests = askOption $ \(Seed mseed) ->
        let gen = mkQCGen $ fromMaybe defaultSeed mseed
        in testGroup "hpqtypes" $ tests (gen, connSettings)
      ingredients = includingOptions [Option $ Proxy @Seed] : defaultIngredients
  finally (withArgs args $ defaultMainWithIngredients ingredients allTests) $ do
    dropStructures connSource
    unless (any ("--seed" `L.isPrefixOf`) args) . putStrLn $
      "Use --seed=" <> show defaultSeed <> " to reproduce this run."
