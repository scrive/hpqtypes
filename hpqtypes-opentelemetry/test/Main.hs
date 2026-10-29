module Main (main) where

import Control.Exception
import Control.Monad
import Data.IORef
import Data.Int
import Data.List qualified as L
import Data.Monoid.Utils
import Data.Text qualified as T
import Data.Vector qualified as V
import Database.PostgreSQL.PQTypes
import OpenTelemetry.Attributes
import OpenTelemetry.Exporter.InMemory.Span
import OpenTelemetry.Trace.Core
import OpenTelemetry.Util
import System.Environment
import System.Exit
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Database.PostgreSQL.PQTypes.OpenTelemetry
import QuerySummaryTest

data RecordedSpan = RecordedSpan
  { rsName :: T.Text
  , rsKind :: SpanKind
  , rsParent :: Maybe T.Text
  , rsAttributes :: Attributes
  , rsStatus :: SpanStatus
  , rsEvents :: [T.Text]
  }

recordSpan :: ImmutableSpan -> IO RecordedSpan
recordSpan imm = do
  hot <- readIORef $ spanHot imm
  parent <- forM (spanParent imm) $ \p -> do
    hotName <$> (unsafeReadSpan p >>= readIORef . spanHot)
  pure
    RecordedSpan
      { rsName = hotName hot
      , rsKind = spanKind imm
      , rsParent = parent
      , rsAttributes = hotAttributes hot
      , rsStatus = hotStatus hot
      , rsEvents = map eventName . V.toList . appendOnlyBoundedCollectionValues $ hotEvents hot
      }

-- | Run the queries in a parent span and return the recorded spans.
withTracing :: T.Text -> (ConnectionSettings -> IO ()) -> IO [RecordedSpan]
withTracing connInfo action = do
  (processor, spansRef) <- inMemoryListExporter
  tp <- createTracerProvider [processor] emptyTracerProviderOptions
  let tracer = hpqtypesTracer tp
      settings =
        defaultConnectionSettings
          { csConnInfo = connInfo
          , csQueryObserver = Just $ queryObserver tracer
          }
  inSpan tracer "parent" defaultSpanArguments $ action settings
  _ <- forceFlushTracerProvider tp Nothing
  mapM recordSpan . reverse =<< readIORef spansRef

runDB :: ConnectionSettings -> DBT IO () -> IO ()
runDB settings = runDBT (unConnectionSource $ simpleSource settings) defaultTransactionSettings

findSpan :: [RecordedSpan] -> T.Text -> IO RecordedSpan
findSpan spans name = case filter ((== name) . rsName) spans of
  [s] -> pure s
  ss -> assertFailure $ "Expected one span named " ++ show name ++ ", got " ++ show (length ss)

assertAttribute :: (Eq a, Show a, ToAttribute a) => RecordedSpan -> T.Text -> a -> Assertion
assertAttribute s key expected =
  assertEqual
    ("Attribute " ++ T.unpack key ++ " of span " ++ T.unpack (rsName s))
    (Just $ toAttribute expected)
    (lookupAttribute (rsAttributes s) key)

assertNoAttribute :: RecordedSpan -> T.Text -> Assertion
assertNoAttribute s key =
  assertEqual
    ("Attribute " ++ T.unpack key ++ " of span " ++ T.unpack (rsName s))
    Nothing
    (lookupAttribute (rsAttributes s) key)

tests :: T.Text -> [Test]
tests connInfo =
  [ querySummaryTest
  , queryTracingTest connInfo
  , failedQueryTracingTest connInfo
  , storedProcedureTracingTest connInfo
  , queryTextRecordingTest connInfo
  ]

queryTracingTest :: T.Text -> Test
queryTracingTest connInfo = testCase "Queries are traced" $ do
  spans <- withTracing connInfo $ \settings -> runDB settings $ do
    runSQL_ "CREATE TEMP TABLE otel_test_ (a INTEGER)"
    runSQL_ "INSERT INTO otel_test_ (a) VALUES (1), (2)"
    runSQL_ $ "SELECT a FROM otel_test_ WHERE a >" <?> (0 :: Int32)

  select <- findSpan spans "SELECT otel_test_"
  assertEqual "Kind" Client (rsKind select)
  assertEqual "Parent" (Just "parent") (rsParent select)
  assertAttribute select "db.system.name" ("postgresql" :: T.Text)
  assertAttribute select "db.query.text" ("SELECT a FROM otel_test_ WHERE a > $1" :: T.Text)
  assertAttribute select "db.query.summary" ("SELECT otel_test_" :: T.Text)
  -- The operation and the collection aren't extracted from the text of
  -- the query.
  assertNoAttribute select "db.operation.name"
  assertNoAttribute select "db.collection.name"
  assertAttribute select "db.response.returned_rows" (2 :: Int)
  assertBool "Namespace is recorded" . (/= Nothing) $
    lookupAttribute (rsAttributes select) "db.namespace"

  insert <- findSpan spans "INSERT otel_test_"
  assertAttribute insert "db.query.text" ("INSERT INTO otel_test_ (a) VALUES (1), (2)" :: T.Text)
  assertNoAttribute insert "db.response.returned_rows"

  _ <- findSpan spans "CREATE TABLE otel_test_"
  _ <- findSpan spans "COMMIT"
  assertBool "Transaction start is traced" $
    any (("BEGIN" `T.isPrefixOf`) . rsName) spans

failedQueryTracingTest :: T.Text -> Test
failedQueryTracingTest connInfo = testCase "Failed queries are traced" $ do
  spans <- withTracing connInfo $ \settings -> do
    eres <- try . runDB settings $ do
      runSQL_ $ "SELECT * FROM otel_nonexistent_ WHERE a =" <?> ("secret" :: T.Text)
    case eres of
      Left DBException {} -> pure ()
      Right () -> assertFailure "Query didn't fail"

  select <- findSpan spans "SELECT otel_nonexistent_"
  assertAttribute select "error.type" ("42P01" :: T.Text)
  assertAttribute select "db.response.status_code" ("42P01" :: T.Text)
  case rsStatus select of
    Error msg -> do
      assertBool ("Status mentions the table: " ++ T.unpack msg) $
        "otel_nonexistent_" `T.isInfixOf` msg
      assertBool ("Status doesn't contain parameters: " ++ T.unpack msg) $
        not ("secret" `T.isInfixOf` msg)
    status -> assertFailure $ "Unexpected status: " ++ show status
  assertEqual "Exception is recorded" ["exception"] (rsEvents select)
  _ <- findSpan spans "ROLLBACK"
  pure ()

storedProcedureTracingTest :: T.Text -> Test
storedProcedureTracingTest connInfo = testCase "Stored procedures are traced" $ do
  spans <- withTracing connInfo $ \settings -> runDB settings $ do
    runSQL_ "CREATE PROCEDURE pg_temp.otel_procedure_(a INTEGER) LANGUAGE SQL AS 'SELECT a'"
    runSQL_ $ "CALL pg_temp.otel_procedure_(" <?> (1 :: Int32) <+> ")"
  call <- findSpan spans "CALL pg_temp.otel_procedure_"
  assertAttribute call "db.stored_procedure.name" ("pg_temp.otel_procedure_" :: T.Text)
  assertAttribute call "db.query.text" ("CALL pg_temp.otel_procedure_( $1 )" :: T.Text)
  assertNoAttribute call "db.collection.name"

queryTextRecordingTest :: T.Text -> Test
queryTextRecordingTest connInfo = testCase "Query text is not recorded if disabled" $ do
  (processor, spansRef) <- inMemoryListExporter
  tp <- createTracerProvider [processor] emptyTracerProviderOptions
  let settings =
        defaultConnectionSettings
          { csConnInfo = connInfo
          , csQueryObserver =
              Just $
                queryObserverWith
                  defaultObserverSettings {osRecordQueryText = False}
                  (hpqtypesTracer tp)
          }
  runDB settings $ runSQL_ "SELECT 1"
  spans <- mapM recordSpan =<< readIORef spansRef
  assertBool "Spans are recorded" . not $ L.null spans
  forM_ spans $ \s -> assertNoAttribute s "db.query.text"

main :: IO ()
main = do
  getArgs >>= \case
    connInfo : args -> defaultMainWithArgs (tests $ T.pack connInfo) args
    [] -> do
      prog <- getProgName
      putStrLn $ "Usage: " ++ prog ++ " <connection info string> [test-framework args]"
      exitFailure
