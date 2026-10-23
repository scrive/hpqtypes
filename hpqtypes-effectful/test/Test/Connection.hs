{-# LANGUAGE OverloadedStrings #-}

-- | Tests of connection handling.
module Test.Connection (connectionTests) where

import Control.Monad
import Data.Typeable
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.MVar
import Effectful.Exception
import Effectful.HPQTypes
import Test.Tasty
import Test.Tasty.HUnit

import Test.Env

connectionTests :: TestData -> [TestTree]
connectionTests td =
  [ testCase "connection stats with a new session" $
      testConnectionStatsWithNewSession td
  , testCase "session thread binding" $
      testSessionThreadBinding td
  ]

testConnectionStatsWithNewSession :: TestData -> Assertion
testConnectionStatsWithNewSession td = runTest td $ do
  runSQL_ "SELECT 1"
  runSQL_ "SELECT 2"
  stats <- getConnectionStats
  liftIO $ assertEqual "Incorrect statsQueries" 2 $ statsQueries stats
  unsafeWithoutTransaction
    . bracket_
      (runSQL_ "CREATE TABLE some_table (field INT)")
      (runSQL_ "DROP TABLE some_table")
    $ do
      withManualTransaction $ do
        runSQL_ "INSERT INTO some_table VALUES (1)"
        withNewSession $ do
          newStats <- getConnectionStats
          liftIO $ assertEqual "Connection stats should be reset" 0 $ statsQueries newStats
          noOfResults <- runSQL "SELECT * FROM some_table"
          liftIO $ assertEqual "Results should not be visible yet" 0 noOfResults
      noOfResults <- runSQL "SELECT * FROM some_table"
      liftIO $ assertEqual "Results should be visible" 1 noOfResults
  where
    -- Without the rollback the DROP TABLE of the enclosing bracket_ runs
    -- inside the failed transaction and is discarded with it.
    withManualTransaction :: DB :> es => Eff es a -> Eff es a
    withManualTransaction action =
      fst
        <$> generalBracket
          (runSQL_ "BEGIN")
          ( \() -> \case
              ExitCaseSuccess _ -> runSQL_ "COMMIT"
              _ -> runSQL_ "ROLLBACK"
          )
          (\() -> action)

testSessionThreadBinding :: TestData -> Assertion
testSessionThreadBinding td = runTest td $ do
  result <- newEmptyMVar
  void . forkIO $ do
    sameSession <- try $ runSQL_ "SELECT 1"
    newSession <- try @SomeException . withNewSession $ runSQL_ "SELECT 1"
    putMVar result (sameSession, newSession)
  (sameSession, newSession) <- takeMVar result
  liftIO $ do
    assertThreadMismatch sameSession
    case newSession of
      Left err ->
        assertFailure $ "withNewSession failed in another thread: " <> show err
      Right () -> pure ()
  where
    assertThreadMismatch :: Either SomeException () -> Assertion
    assertThreadMismatch = \case
      Left err
        | Just (DBException _ _ specificError _) <- fromException err
        , Just ThreadMismatchError {} <- cast specificError ->
            pure ()
        | otherwise ->
            assertFailure $ "The query threw an unexpected exception: " <> show err
      Right () -> assertFailure "The query did not throw ThreadMismatchError"
