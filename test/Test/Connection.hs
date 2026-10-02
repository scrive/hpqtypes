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
import Data.ByteString qualified as BS
import Data.Int
import Data.List qualified as L
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
  , notifyEncodingTest td
  , queryInterruptionTest td
  , largeQueryInterruptionTest td
  , preparedQueryInterruptionTest td
  , syncExceptionInterruptionTest td
  , copyNotSupportedTest td
  , onDemandTest td
  , onDemandDeadConnectionTest td
  , sessionOwnerTest td
  , childSessionTest td
  , acquisitionModeChangeFailureTest td
  ]

autocommitTest :: TestData -> TestTree
autocommitTest td = testCase "Autocommit mode works"
  . runTestEnv td defaultTransactionSettings
  . unsafeWithoutTransaction
  $ do
    let sint = Identity (1 :: Int32)
    runQuery_ $ rawSQL "INSERT INTO test1_ (a) VALUES ($1)" sint
    withNewSession $ do
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
        td.connSettings
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
    -- A prepared statement rejects a change of a parameter type.
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
  forkNewSession $ notify chan payload
  mnt1 <- getNotification 250000
  liftBase $ assertBool "Notification received" (isJust mnt1)
  Just nt1 <- pure mnt1
  assertEqual "Channels are equal" chan (ntChannel nt1)
  assertEqual "Payloads are equal" payload (ntPayload nt1)

  unlisten chan
  forkNewSession $ notify chan payload
  mnt2 <- getNotification 250000
  assertEqual "No notification received after unlisten" Nothing mnt2

  listen chan
  unlistenAll
  forkNewSession $ notify chan payload
  mnt3 <- getNotification 250000
  assertEqual "No notification received after unlistenAll" Nothing mnt3

  mnt4 <- timeout 100000 $ getNotification 10000000
  assertEqual "Outer timeout interrupts the wait" Nothing mnt4
  where
    chan = "test_channel"
    payload = "test_payload"
    forkNewSession = void . fork . withNewSession

-- | The server converts the payload of a notification to the client encoding
-- of the listener. With a client encoding other than UTF-8, the payload can
-- be invalid UTF-8.
notifyEncodingTest :: TestData -> TestTree
notifyEncodingTest td = testCase "Notification payload in invalid UTF-8 is rejected"
  . runTestEnv td defaultTransactionSettings
  . unsafeWithoutTransaction
  $ do
    runSQL_ "SET client_encoding TO 'LATIN1'"
    listen chan
    -- The payload is "café", built on the server so that the client encoding
    -- of the sender does not matter. A character without a LATIN1 equivalent
    -- would make the server close the connection of the listener instead.
    void . fork . withNewSession $
      runSQL_ "SELECT pg_notify('latin1_channel', convert_from('\\x636166c3a9', 'UTF8'))"
    expectError @HPQTypesError "payload in LATIN1" check $ getNotification 250000
    runSQL_ "RESET client_encoding"
    unlisten chan
  where
    chan = "latin1_channel"

    check :: HPQTypesError -> TestEnv ()
    check (HPQTypesError msg) =
      liftBase . assertBool ("Error names the channel: " ++ msg) $
        "payload on channel \"latin1_channel\" is not valid UTF-8" `L.isInfixOf` msg

queryInterruptionTest :: TestData -> TestTree
queryInterruptionTest td = testCase "Queries are interruptible" $ do
  let sleep = "SELECT pg_sleep(2)"
      -- Both queries must take much longer than the timeout below. This keeps
      -- a query that ran to completion distinguishable from an interrupted one.
      ints = sqlGenInts 10000000
  runTestEnv td defaultTransactionSettings . unsafeWithoutTransaction $ do
    testQuery id sleep
    testQuery id ints
  runTestEnv td defaultTransactionSettings $ do
    testQuery (withSavepoint "ints") ints
    testQuery (withSavepoint "sleep") sleep
  where
    testQuery m sql = do
      (interrupted, elapsed) <- timed . timeout 500000 . m $ runSQL_ sql
      when (isJust interrupted) . liftBase $ do
        assertFailure $ "Query" <+> show sql <+> "wasn't interrupted in time"
      -- A fired timeout alone proves nothing. If execution doesn't run in a
      -- thread of its own, the exception arrives only after the blocking call
      -- returns. The timeout would then report an interruption after the
      -- whole query ran to completion.
      liftBase . assertBool ("Query" <+> show sql <+> "was waited out") $
        elapsed < 1

-- | A query is interruptible while the server executes it, but also while it
-- is still on its way there. The second case is a separate matter. The server
-- discards a cancellation request that arrives before the query it targets, so
-- a single request is not enough. Without a repeated request, an exception
-- that arrives during the transmission of a large parameter leaves the query
-- running. The interrupted thread then waits for the query to complete.
--
-- The parameter is large enough for its transmission to take much longer than
-- the timeout below. The timeout therefore fires in the middle of it.
--
-- Cancellation cannot be quicker than the end of the transmission. The test
-- therefore measures the time it takes to send the parameter instead of
-- assuming it. The time depends on how the server is reached. A local socket
-- is orders of magnitude faster than a connection to another host. Next to a
-- hardcoded bound, an interruption that is as prompt as it can be would look
-- like a stall.
largeQueryInterruptionTest :: TestData -> TestTree
largeQueryInterruptionTest td =
  testCase "Queries are interruptible while being sent"
    . runTestEnv td defaultTransactionSettings
    . unsafeWithoutTransaction
    $ do
      let payload = BS.replicate (4 * 1024 * 1024) 65
          sleepSeconds = 2 :: Double
          sql =
            "SELECT pg_sleep(" <?> sleepSeconds <> "), length(" <?> payload <> ")"
      (_, sendTime) <- timed . runSQL_ $ "SELECT length(" <?> payload <> ")"
      replicateM_ 10 $ do
        (interrupted, elapsed) <- timed . timeout 2000 $ runSQL_ sql
        when (isJust interrupted) . liftBase $
          assertFailure "Query wasn't interrupted in time"
        -- If the query ran to completion, it would take the sleep above on top
        -- of the transmission. A time close to the transmission alone means
        -- the query was cancelled.
        liftBase . assertBool "Query was waited out rather than cut short" $
          elapsed < sendTime + sleepSeconds / 2
      -- Cancellation must leave the connection in a state in which it can run
      -- queries again.
      runSQL_ "SELECT 1::int4"
      n <- fetchOne (fromSQL @Int32)
      assertEqual "Connection is usable after the interruption" 1 n

-- | Execution of a prepared statement must be interruptible like that of a
-- regular query. The test is specifically about the execution. Preparation is
-- deliberately not interruptible, so the test prepares the statement upfront
-- to keep it out of the picture.
preparedQueryInterruptionTest :: TestData -> TestTree
preparedQueryInterruptionTest td = testCase "Prepared queries are interruptible"
  . runTestEnv td defaultTransactionSettings
  . unsafeWithoutTransaction
  $ do
    sleep 0
    (interrupted, elapsed) <- timed . timeout 500000 $ sleep 2
    when (isJust interrupted) . liftBase $
      assertFailure "Prepared query wasn't interrupted in time"
    -- A fired timeout alone proves nothing. If execution doesn't run in a
    -- thread of its own, the exception arrives only after the blocking call
    -- returns. The timeout would then report an interruption after the whole
    -- query ran to completion.
    liftBase . assertBool "Prepared query was waited out rather than cut short" $
      elapsed < 1
    -- Cancellation must leave the connection in a state in which it can run
    -- queries again.
    runSQL_ "SELECT 1::int4"
    n <- fetchOne (fromSQL @Int32)
    assertEqual "Connection is usable after the interruption" 1 n
  where
    -- The duration is a parameter, so both calls execute the same statement.
    -- The second call finds it already prepared.
    sleep :: Double -> TestEnv ()
    sleep n = runPreparedQuery_ "sleep" $ rawSQL "SELECT pg_sleep($1)" (Identity n)

syncExceptionInterruptionTest :: TestData -> TestTree
syncExceptionInterruptionTest td = testCase
  "Query interrupted by an exception of a synchronous type is cancelled"
  . runTestEnv td defaultTransactionSettings
  . unsafeWithoutTransaction
  $ do
    -- throwTo delivers an exception of a synchronous type (e.g. ExitCode from
    -- a shutdown handler) from another thread the same way as an asynchronous
    -- one. Such an exception must also cancel the query and leave the
    -- connection idle. Code that catches it can then keep running queries.
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

copyNotSupportedTest :: TestData -> TestTree
copyNotSupportedTest td =
  testGroup
    "COPY statements fail with an error"
    [ check "in the acquire and hold mode" defaultTransactionSettings
    , -- In the on demand mode the query runs in an automatic transaction. Its
      -- ROLLBACK fails as well, because the connection is in a copy mode. The
      -- failure must not mask the error of the COPY statement itself.
      check "in the on demand mode" $
        defaultTransactionSettings {tsConnectionAcquisitionMode = AcquireOnDemand}
    ]
  where
    ConnectionSource copyCs = simpleSource td.connSettings

    -- The failed COPY statement leaves the connection in a copy mode. The
    -- connection can't run further queries, so the test uses a dedicated one.
    check name ts = testCase name $ do
      eres <- try . runDBT copyCs ts $ do
        runSQL_ "COPY (SELECT 1) TO STDOUT"
      case eres of
        Left DBException {dbeError = err} -> case fromException $ toException err of
          Just (HPQTypesError msg) ->
            assertBool ("Error message mentions COPY: " ++ msg) $
              "COPY" `T.isInfixOf` T.pack msg
          Nothing -> assertFailure $ "Unexpected error: " ++ show err
        Right () -> assertFailure "COPY statement didn't fail"

onDemandDeadConnectionTest :: TestData -> TestTree
onDemandDeadConnectionTest td = testCase
  "Failed ROLLBACK of an on demand transaction doesn't mask the query error"
  . runTestEnv td ts
  $ do
    -- The query kills its own backend. The ROLLBACK that ends the automatic
    -- transaction fails as well, because the connection is gone. The error of
    -- the query must propagate regardless.
    eres <- try $ runSQL_ "SELECT pg_terminate_backend(pg_backend_pid())"
    liftBase $ case eres of
      Left DBException {..} ->
        assertBool ("Exception comes from the query: " ++ show dbeQueryContext) $
          "pg_terminate_backend" `L.isInfixOf` show dbeQueryContext
      Right () -> assertFailure "DBException wasn't thrown"
  where
    ts = defaultTransactionSettings {tsConnectionAcquisitionMode = AcquireOnDemand}

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
  -- The second switch makes sure that the operation is idempotent.
  acquireAndHoldConnection DefaultLevel DefaultPermissions
  runSQL_ $ "INSERT INTO test1_ (a) VALUES (" <?> v <+> ")"

  unsafeAcquireOnDemandConnection
  runSQL_ "SHOW transaction_read_only"
  "on" <- fetchOne $ fromSQL @T.Text
  -- The second switch makes sure that the operation is idempotent.
  unsafeAcquireOnDemandConnection
  n <- runSQL $ "SELECT TRUE FROM test1_ WHERE a =" <?> v
  assertEqual "Unexpected amount of rows" 1 n
  where
    ts = defaultTransactionSettings {tsConnectionAcquisitionMode = AcquireOnDemand}

    v :: Int32
    v = 1337

sessionOwnerTest :: TestData -> TestTree
sessionOwnerTest td =
  testCase "Only the thread that owns a DB session can use it"
    . runTestEnv td defaultTransactionSettings
    $ do
      result <- newEmptyMVar
      _ <- fork $ do
        sameSessionQuery <- try $ runSQL_ "SELECT 1"
        sameSessionModeChange <- try unsafeAcquireOnDemandConnection
        newSession <- try . withNewSession $ runSQL_ "SELECT 1"
        putMVar result (sameSessionQuery, sameSessionModeChange, newSession)
      (sameSessionQuery, sameSessionModeChange, newSession) <- takeMVar result
      liftBase $ do
        assertThreadMismatch "runSQL_" sameSessionQuery
        assertThreadMismatch "unsafeAcquireOnDemandConnection" sameSessionModeChange
        case newSession of
          Left (e :: SomeException) ->
            assertFailure $ "withNewSession failed in another thread: " ++ show e
          Right () -> pure ()
  where
    assertThreadMismatch :: String -> Either SomeException () -> Assertion
    assertThreadMismatch op = \case
      Left e
        | Just DBException {..} <- fromException e
        , Just ThreadMismatchError {} <- cast dbeError ->
            pure ()
        | otherwise -> assertFailure $ op ++ " threw an unexpected exception: " ++ show e
      Right () -> assertFailure $ op ++ " didn't throw ThreadMismatchError"

childSessionTest :: TestData -> TestTree
childSessionTest td = testCase
  "Child thread can start a session after the parent session ended"
  $ do
    parentEnded <- newEmptyMVar
    result <- newEmptyMVar
    runTestEnv td defaultTransactionSettings $ do
      void . fork $ do
        takeMVar parentEnded
        putMVar result =<< try (withNewSession $ runSQL_ "SELECT 1")
    putMVar parentEnded ()
    takeMVar result >>= \case
      Left (e :: SomeException) ->
        assertFailure $ "withNewSession failed in the child thread: " ++ show e
      Right () -> pure ()

acquisitionModeChangeFailureTest :: TestData -> TestTree
acquisitionModeChangeFailureTest td = testCase
  "Connection state is usable after a failed acquisition mode change"
  . runTestEnv td defaultTransactionSettings
  $ do
    -- The deferred constraint violation makes the COMMIT issued by
    -- unsafeAcquireOnDemandConnection fail.
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
    -- connection state must be on demand now. In particular, it must not refer
    -- to the connection that is already gone.
    mode <- getConnectionAcquisitionMode
    assertEqual "Unexpected connection acquisition mode" AcquireOnDemand mode
    runSQL_ "SELECT 1"
    n <- fetchOne (fromSQL @Int32)
    assertEqual "Unexpected query result" 1 n
