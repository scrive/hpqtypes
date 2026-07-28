-- | Tests of connection management: autocommit mode, roles, prepared
-- statements, notifications, query interruption and acquisition modes.
module Test.Connection
  ( connectionTests
  ) where

import Control.Concurrent.Lifted
import Control.Exception (ErrorCall (..))
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Data.Int
import Data.Maybe
import Data.Text qualified as T
import Data.Typeable
import System.Timeout.Lifted
import Test.Tasty

import Data.Monoid.Utils
import Database.PostgreSQL.PQTypes
import Test.Env

connectionTests :: TestData -> [TestTree]
connectionTests td =
  [ autocommitTest td
  , setRoleTest td
  , preparedStatementTest td
  , notifyTest td
  , queryInterruptionTest td
  , syncExceptionInterruptionTest td
  , finalizationInterruptionTest td
  , copyNotSupportedTest td
  , onDemandTest td
  , acquisitionModeChangeFailureTest td
  ]

autocommitTest :: TestData -> TestTree
autocommitTest td = testCase "Autocommit mode works"
  . runTestEnv td defaultTransactionSettings
  . unsafeWithoutTransaction
  $ do
    let sint = Identity (1 :: Int32)
    runQuery_ $ rawSQL "INSERT INTO test1_ (a) VALUES ($1)" sint
    withNewConnection $ do
      n <- runQuery $ rawSQL "SELECT a FROM test1_ WHERE a = $1" sint
      assertEqual "Other connection sees autocommited data" 1 n
    runQuery_ $ rawSQL "DELETE FROM test1_ WHERE a = $1" sint

setRoleTest :: TestData -> TestTree
setRoleTest td = testCase "SET ROLE works" . bracket createRole dropRole $ \case
  False -> putStrLn "Cannot create role, skipping SET ROLE test"
  True -> do
    runDBT roledCs defaultTransactionSettings $ do
      runSQL_ "SELECT CURRENT_USER::text"
      role <- fetchOne (fromSQL @String)
      assertEqual "Role set successfully" testRole role
  where
    testRole :: String
    testRole = "hpqtypes_test_role"

    ConnectionSource roledCs =
      simpleSource $
        (snd td)
          { csRole = Just $ unsafeSQL testRole
          }

    createRole = runTestEnv td defaultTransactionSettings $ do
      try (runSQL_ $ "CREATE ROLE" <+> unsafeSQL testRole) >>= \case
        Right () -> pure True
        Left DBException {} -> pure False

    dropRole = \case
      False -> pure ()
      True -> runTestEnv td defaultTransactionSettings $ do
        runSQL_ $ "DROP ROLE" <+> unsafeSQL testRole

preparedStatementTest :: TestData -> TestTree
preparedStatementTest td = testCase "Execution of prepared statements works"
  . runTestEnv td defaultTransactionSettings
  $ do
    let name = "select1"

    checkPrepared name "Statement is not prepared" 0
    execPrepared name 42
    checkPrepared name "Statement is prepared" 1
    execPrepared name 89

    let i3 = "lalala" :: String
    -- Changing parameter type in an already prepared statement shouldn't work.
    o3 <- try . runPreparedQuery_ name $ ("SELECT" <?> i3)
    case o3 of
      Left DBException {} -> pure ()
      Right r3 -> liftBase . assertFailure $ "Expected DBException, but got" <+> show r3
  where
    checkPrepared :: QueryName -> String -> Int -> TestEnv ()
    checkPrepared (QueryName name) assertTitle expected = do
      n <- runSQL $ "SELECT TRUE FROM pg_prepared_statements WHERE name =" <?> name
      assertEqual assertTitle expected n

    execPrepared :: QueryName -> Int32 -> TestEnv ()
    execPrepared name input = do
      runPreparedQuery_ name $ "SELECT" <?> input
      output <- fetchOne fromSQL
      assertEqual "Results match" input output

notifyTest :: TestData -> TestTree
notifyTest td = testCase "Notifications work" . runTestEnv td defaultTransactionSettings . unsafeWithoutTransaction $ do
  listen chan
  forkNewConn $ notify chan payload
  mnt1 <- getNotification 250000
  liftBase $ assertBool "Notification received" (isJust mnt1)
  Just nt1 <- pure mnt1
  assertEqual "Channels are equal" chan (ntChannel nt1)
  assertEqual "Payloads are equal" payload (ntPayload nt1)

  unlisten chan
  forkNewConn $ notify chan payload
  mnt2 <- getNotification 250000
  assertEqual "No notification received after unlisten" Nothing mnt2

  listen chan
  unlistenAll
  forkNewConn $ notify chan payload
  mnt3 <- getNotification 250000
  assertEqual "No notification received after unlistenAll" Nothing mnt3
  where
    chan = "test_channel"
    payload = "test_payload"
    forkNewConn action = do
      sem <- newEmptyMVar
      void . fork . withNewConnection $ do
        -- withNewConnection needs access to the connection state to get current
        -- ConnectionAcquisitionMode, but getNotification called immediately
        -- after takes ownership of the connection state for its duration, so if
        -- CPU gets to it first, withNewConnection will block and notification
        -- will never be sent.
        putMVar sem ()
        action
      takeMVar sem

queryInterruptionTest :: TestData -> TestTree
queryInterruptionTest td = testCase "Queries are interruptible" $ do
  let sleep = "SELECT pg_sleep(2)"
      ints = sqlGenInts 5000000
  runTestEnv td defaultTransactionSettings . unsafeWithoutTransaction $ do
    testQuery id sleep
    testQuery id ints
  runTestEnv td defaultTransactionSettings $ do
    testQuery (withSavepoint "ints") ints
    testQuery (withSavepoint "sleep") sleep
  where
    testQuery m sql =
      timeout 500000 (m $ runSQL_ sql) >>= \case
        Just _ -> liftBase $ do
          assertFailure $ "Query" <+> show sql <+> "wasn't interrupted in time"
        Nothing -> pure ()

syncExceptionInterruptionTest :: TestData -> TestTree
syncExceptionInterruptionTest td = testCase
  "Query interrupted by an exception of a synchronous type is cancelled"
  . runTestEnv td defaultTransactionSettings
  . unsafeWithoutTransaction
  $ do
    -- throwTo delivers exceptions of synchronous types (e.g. ExitCode from a
    -- shutdown handler) from other threads the same way as asynchronous
    -- ones, so they too must cancel the query and drain the connection of
    -- its results, so that code that catches such an exception can keep
    -- running queries.
    tid <- myThreadId
    void . fork $ do
      threadDelay 100000
      throwTo tid $ ErrorCall "interrupt"
    interrupted <-
      timeout 1000000 . expectError @ErrorCall "interrupted query" (\_ -> pure ()) $
        runSQL_ "SELECT pg_sleep(2)"
    when (isNothing interrupted) . liftBase $
      assertFailure "Query wasn't cancelled in time"
    runSQL_ "SELECT 1::int4"
    n <- fetchOne (fromSQL @Int32)
    assertEqual "Connection is usable after the interruption" 1 n

finalizationInterruptionTest :: TestData -> TestTree
finalizationInterruptionTest td = testCase
  "Interrupted connection finalization doesn't deadlock other threads"
  $ do
    queryDone <- newEmptyMVar
    -- Exit the runDBT scope while the forked thread is still using the
    -- connection, so that the finalization blocks on the connection state
    -- MVar, and interrupt it with a timeout.
    eres <- timeout 500000 . runTestEnv td defaultTransactionSettings $ do
      _ <- fork $ do
        runSQL_ "SELECT pg_sleep(2)"
        putMVar queryDone ()
      threadDelay 200000
    case eres of
      Just _ -> assertFailure "Connection finalization wasn't interrupted in time"
      Nothing -> pure ()
    -- The forked thread needs to be able to finish, i.e. to return the
    -- connection state to the MVar once its query completes.
    timeout 5000000 (takeMVar queryDone) >>= \case
      Just () -> pure ()
      Nothing -> assertFailure "Forked thread deadlocked on the connection state MVar"

copyNotSupportedTest :: TestData -> TestTree
copyNotSupportedTest td = testCase "COPY statements fail with an error" $ do
  -- The failed COPY statement leaves the connection in a copy mode, making
  -- it unusable for further queries, so run it on a dedicated connection.
  eres <- try . runDBT copyCs defaultTransactionSettings $ do
    runSQL_ "COPY (SELECT 1) TO STDOUT"
  case eres of
    Left DBException {dbeError = err} -> case fromException $ toException err of
      Just (HPQTypesError msg) ->
        assertBool ("Error message mentions COPY: " ++ msg) $
          "COPY" `T.isInfixOf` T.pack msg
      Nothing -> assertFailure $ "Unexpected error: " ++ show err
    Right () -> assertFailure "COPY statement didn't fail"
  where
    ConnectionSource copyCs = simpleSource $ snd td

onDemandTest :: TestData -> TestTree
onDemandTest td = testCase "OnDemand mode works" . runTestEnv td ts $ do
  runSQL_ "SELECT a FROM test1_"
  _ <- fetchMany (fromSQL @Int32)

  er <- try . runSQL_ $ "INSERT INTO test1_ (a) VALUES (" <?> v <+> ")"
  liftBase $ case er of
    Left DBException {..}
      | Just DetailedQueryError {..} <- cast dbeError -> do
          assertEqual "Unexpected error code" ReadOnlySqlTransaction qeErrorCode
      | otherwise -> assertFailure $ "Unexpected exception: " ++ show dbeError
    Right () -> assertFailure "DBException wasn't thrown"

  acquireAndHoldConnection DefaultLevel DefaultPermissions
  runSQL_ "SHOW transaction_read_only"
  "off" <- fetchOne $ fromSQL @T.Text
  -- Switch twice to check idempotency.
  acquireAndHoldConnection DefaultLevel DefaultPermissions
  runSQL_ $ "INSERT INTO test1_ (a) VALUES (" <?> v <+> ")"

  unsafeAcquireOnDemandConnection
  runSQL_ "SHOW transaction_read_only"
  "on" <- fetchOne $ fromSQL @T.Text
  -- Switch twice to check idempotency.
  unsafeAcquireOnDemandConnection
  n <- runSQL $ "SELECT TRUE FROM test1_ WHERE a =" <?> v
  assertEqual "Unexpected amount of rows" 1 n
  where
    ts = defaultTransactionSettings {tsConnectionAcquisitionMode = AcquireOnDemand}

    v :: Int32
    v = 1337

acquisitionModeChangeFailureTest :: TestData -> TestTree
acquisitionModeChangeFailureTest td = testCase
  "Connection state is usable after a failed acquisition mode change"
  . runTestEnv td defaultTransactionSettings
  $ do
    -- Violate a deferred constraint so that the COMMIT issued by
    -- unsafeAcquireOnDemandConnection fails.
    runSQL_ "CREATE TABLE mode_change_ (a INTEGER UNIQUE DEFERRABLE INITIALLY DEFERRED)"
    runSQL_ "INSERT INTO mode_change_ (a) VALUES (1), (1)"
    eres <- try unsafeAcquireOnDemandConnection
    liftBase $ case eres of
      Left DBException {..}
        | Just DetailedQueryError {..} <- cast dbeError -> do
            assertEqual "Unexpected error code" UniqueViolation qeErrorCode
        | otherwise -> assertFailure $ "Unexpected exception: " ++ show dbeError
      Right () -> assertFailure "DBException wasn't thrown"

    -- The failed COMMIT returned the connection to its source, so the
    -- connection state needs to be on demand now. In particular, it must not
    -- refer to the connection that is already gone.
    mode <- getConnectionAcquisitionMode
    assertEqual "Unexpected connection acquisition mode" AcquireOnDemand mode
    runSQL_ "SELECT 1"
    n <- fetchOne (fromSQL @Int32)
    assertEqual "Unexpected query result" 1 n
