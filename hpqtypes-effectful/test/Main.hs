{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Pool
import Data.Text qualified as T
import Effectful.HPQTypes
import System.Environment
import System.Exit
import Test.Tasty

import Test.Connection
import Test.Env
import Test.LastQuery

tests :: TestData -> [TestTree]
tests td =
  concat
    [ lastQueryTests td
    , connectionTests td
    ]

main :: IO ()
main = do
  (connString, args) <- getConnString
  let connSettings = defaultConnectionSettings {csConnInfo = connString}
  connSource <- poolSource connSettings $ \connect disconnect ->
    defaultPoolConfig connect disconnect cacheTTL maxConnections
  let td = TestData {tdConnSource = unConnectionSource connSource}
  withArgs args . defaultMain . testGroup "hpqtypes-effectful" $ tests td
  where
    -- For parallel execution of tests.
    maxConnections :: Int
    maxConnections = 16

    cacheTTL :: Double
    cacheTTL = 10

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
