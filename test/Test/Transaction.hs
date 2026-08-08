-- | Tests of transaction handling: isolation levels, read only mode,
-- savepoints, restarts and failed commits.
module Test.Transaction
  ( transactionTests
  ) where

import Control.Concurrent.Lifted
import Control.Exception (ErrorCall (..), MaskingState (..), getMaskingState)
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Data.Int
import Data.Typeable
import System.Timeout.Lifted
import Test.Tasty

import Data.Monoid.Utils
import Database.PostgreSQL.PQTypes
import Test.Env

transactionTests :: TestData -> [TestTree]
transactionTests td =
  [ readOnlyTest td
  , savepointTest td
  , transactionTest td ReadCommitted
  , transactionTest td RepeatableRead
  , transactionTest td Serializable
  , commitFailureTest td
  , restartTest td
  ]

readOnlyTest :: TestData -> TestTree
readOnlyTest td = testCase "Read only transaction mode works"
  . runTestEnv
    td
    defaultTransactionSettings {tsConnectionAcquisitionMode = AcquireAndHold DefaultLevel ReadOnly}
  $ do
    let sint = Identity (2 :: Int32)
    eres <- try . runQuery_ $ rawSQL "INSERT INTO test1_ (a) VALUES ($1)" sint
    case eres :: Either DBException () of
      Left _ -> pure ()
      Right _ -> liftBase . assertFailure $ "DBException wasn't thrown"
    rollback
    n <- runQuery $ rawSQL "SELECT a FROM test1_ WHERE a = $1" sint
    assertEqual "SELECT works in read only mode" 0 n

savepointTest :: TestData -> TestTree
savepointTest td = testCase "Savepoint support works"
  . runTestEnv td defaultTransactionSettings
  $ do
    let int1 = 3 :: Int32
        int2 = 4 :: Int32

    -- action executed within withSavepoint throws
    runQuery_ $ rawSQL "INSERT INTO test1_ (a) VALUES ($1)" (Identity int1)
    _ :: Either DBException () <- try . withSavepoint (Savepoint "test") $ do
      runQuery_ $ rawSQL "INSERT INTO test1_ (a) VALUES ($1)" (Identity int2)
      runSQL_ "SELECT * FROM table_that_is_not_there"
    runQuery_ $ rawSQL "SELECT a FROM test1_ WHERE a IN ($1, $2)" (int1, int2)
    res1 <- fetchMany fromSQL
    assertEqual "Part of transaction was rolled back" [int1] res1

    rollback

    -- action executed within withSavepoint doesn't throw
    runQuery_ $ rawSQL "INSERT INTO test1_ (a) VALUES ($1)" (Identity int1)
    withSavepoint (Savepoint "test") $ do
      runQuery_ $ rawSQL "INSERT INTO test1_ (a) VALUES ($1)" (Identity int2)
    runQuery_ $
      rawSQL
        "SELECT a FROM test1_ WHERE a IN ($1, $2) ORDER BY a"
        (int1, int2)
    res2 <- fetchMany fromSQL
    assertEqual "Result of all queries is visible" [int1, int2] res2

transactionTest :: TestData -> IsolationLevel -> TestTree
transactionTest td lvl =
  testCase
    ( "Auto transaction works by default with isolation level"
        <+> show lvl
    )
    . runTestEnv
      td
      defaultTransactionSettings {tsConnectionAcquisitionMode = AcquireAndHold lvl DefaultPermissions}
    $ do
      let sint = Identity (5 :: Int32)
      runQuery_ $ rawSQL "INSERT INTO test1_ (a) VALUES ($1)" sint
      withNewConnection $ do
        n <- runQuery $ rawSQL "SELECT a FROM test1_ WHERE a = $1" sint
        assertEqual "Other connection doesn't see uncommited data" 0 n
      rollback

commitFailureTest :: TestData -> TestTree
commitFailureTest td = testCase
  "Transaction is active after a failed commit"
  . runTestEnv td defaultTransactionSettings
  $ do
    runSQL_ "CREATE TABLE commit_failure_ (a INTEGER UNIQUE DEFERRABLE INITIALLY DEFERRED)"
    commit
    (`finally` cleanup) $ do
      -- Deferred constraint violation makes the COMMIT fail.
      runSQL_ "INSERT INTO commit_failure_ (a) VALUES (1), (1)"
      eres <- try commit
      liftBase $ case eres :: Either DBException () of
        Left DBException {..}
          | Just DetailedQueryError {..} <- cast dbeError -> do
              assertEqual "Unexpected error code" UniqueViolation qeErrorCode
          | otherwise -> assertFailure $ "Unexpected exception: " ++ show dbeError
        Right () -> assertFailure "DBException wasn't thrown"
      -- A new transaction needs to be active at this point, so a write made
      -- after the failed commit must be reverted by a rollback.
      runSQL_ "INSERT INTO commit_failure_ (a) VALUES (2)"
      rollback
      n <- runSQL "SELECT a FROM commit_failure_"
      assertEqual "Unexpected number of rows" 0 n
  where
    cleanup = do
      rollback
      runSQL_ "DROP TABLE commit_failure_"
      commit

restartTest :: TestData -> TestTree
restartTest td =
  testGroup
    "Transaction restarts"
    [ restartedTransactionIsNotMasked
    , asyncExceptionsDontTriggerRestarts
    ]
  where
    restartedTransactionIsNotMasked = testCase
      "Restarted transaction doesn't run with asynchronous exceptions masked"
      $ do
        let ts =
              defaultTransactionSettings
                { tsRestartPredicate = Just . RestartPredicate $ \(e :: ErrorCall) _ ->
                    e == ErrorCall "restart"
                }
        attempts <- newMVar (0 :: Int)
        runTestEnv td ts $ do
          n <- modifyMVar attempts $ \n -> pure (n + 1, n + 1)
          when (n == 1) . throwM $ ErrorCall "restart"
          ms <- liftBase getMaskingState
          assertEqual "Unexpected masking state" Unmasked ms

    asyncExceptionsDontTriggerRestarts = testCase
      "Asynchronous exceptions don't trigger a transaction restart"
      $ do
        let ts =
              defaultTransactionSettings
                { tsRestartPredicate = Just . RestartPredicate $ \(_ :: SomeException) n ->
                    n < 3
                }
        timeout 500000 (runTestEnv td ts $ runSQL_ "SELECT pg_sleep(2)") >>= \case
          Just _ -> assertFailure "Query wasn't interrupted in time"
          Nothing -> pure ()
