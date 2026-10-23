{-# LANGUAGE OverloadedStrings #-}

-- | Tests of the query recorded as the last one.
module Test.LastQuery (lastQueryTests) where

import Data.Int
import Effectful
import Effectful.HPQTypes
import Test.Tasty
import Test.Tasty.HUnit

import Test.Env

lastQueryTests :: TestData -> [TestTree]
lastQueryTests td =
  [ testCase "getLastQuery" $ testGetLastQuery td
  , testCase "withFrozenLastQuery" $ testWithFrozenLastQuery td
  ]

testGetLastQuery :: TestData -> Assertion
testGetLastQuery td = runTest td $ do
  do
    -- Run the first query and perform some basic sanity checks
    let sql = "SELECT 1"
    rowNo <- runSQL sql
    liftIO $ assertEqual "One row should be retrieved" 1 rowNo
    result <- fetchMany (runIdentity @Int32)
    liftIO $ assertEqual "Result should be [1]" [1] result
    (_, SomeSQL lastQuery) <- getLastQuery
    liftIO $ assertEqual "SQL don't match" (show sql) (show lastQuery)
  do
    -- Run the second query and check that `getLastQuery` gives updated result
    let newSQL = "SELECT 2"
    runSQL_ newSQL
    (_, SomeSQL newLastQuery) <- getLastQuery
    liftIO $ assertEqual "SQL don't match" (show newSQL) (show newLastQuery)

testWithFrozenLastQuery :: TestData -> Assertion
testWithFrozenLastQuery td = runTest td $ do
  let sql = "SELECT 1"
  runSQL_ sql
  withFrozenLastQuery $ do
    runSQL_ "SELECT 2"
    (_, SomeSQL lastQuery) <- getLastQuery
    liftIO $ assertEqual "The last query before freeze should be reported" (show sql) (show lastQuery)
  (_, SomeSQL lastQuery) <- getLastQuery
  liftIO $ assertEqual "The last query before freeze should be reported" (show sql) (show lastQuery)
