-- | OpenTelemetry tracing of the queries that hpqtypes runs.
--
-- Set the observer in the connection settings to create a span for each query
-- of the connection:
--
-- @
-- tracerProvider <- 'getGlobalTracerProvider'
-- let settings =
--       'defaultConnectionSettings'
--         { 'csConnInfo' = \"dbname=example\"
--         , 'csQueryObserver' = Just . 'queryObserver' $ 'hpqtypesTracer' tracerProvider
--         }
-- @
--
-- Spans follow the
-- <https://opentelemetry.io/docs/specs/semconv/database/ semantic conventions for database client spans>.
-- Each span is a child of the span that is active in the thread that runs the
-- query.
module Database.PostgreSQL.PQTypes.OpenTelemetry
  ( -- * Observer
    queryObserver
  , queryObserverWith
  , ObserverSettings (..)
  , defaultObserverSettings

    -- * Tracer
  , hpqtypesTracer
  ) where

import Control.Exception qualified as E
import Data.HashMap.Strict qualified as H
import Data.Maybe
import Data.Text qualified as T
import Data.Typeable
import Data.Version
import Database.PostgreSQL.PQTypes
import Database.PostgreSQL.PQTypes.Observer
import GHC.Stack
import OpenTelemetry.Attributes.Map (AttributeMap)
import OpenTelemetry.Context.ThreadLocal
import OpenTelemetry.SemanticsConfig
import OpenTelemetry.Trace.Core
import Text.Read

import Database.PostgreSQL.PQTypes.OpenTelemetry.QuerySummary
import Paths_hpqtypes_opentelemetry qualified as Paths

-- | Settings of the observer.
newtype ObserverSettings = ObserverSettings
  { osRecordQueryText :: Bool
  -- ^ Whether to record the text of the query as @db.query.text@. The text
  -- doesn't contain values of parameters, since they are sent separately, but
  -- it contains values that the query has as literals.
  }

-- | Default settings of the observer. The text of the query is recorded.
defaultObserverSettings :: ObserverSettings
defaultObserverSettings =
  ObserverSettings
    { osRecordQueryText = True
    }

-- | Tracer of this library from a 'TracerProvider'.
hpqtypesTracer :: TracerProvider -> Tracer
hpqtypesTracer tp = makeTracer tp library tracerOptions
  where
    library =
      instrumentationLibrary
        "hpqtypes-opentelemetry"
        (T.pack $ showVersion Paths.version)

-- | Observer that creates a span for each query with 'defaultObserverSettings'.
queryObserver :: Tracer -> QueryObserver
queryObserver = queryObserverWith defaultObserverSettings

-- | Observer that creates a span for each query.
queryObserverWith :: ObserverSettings -> Tracer -> QueryObserver
queryObserverWith settings tracer = QueryObserver $ do
  -- The query runs in another thread, so the context of the calling thread
  -- needs to be captured here.
  ctx <- getContext
  pure $ QueryObservation $ \qi run ->
    if not (tracerIsEnabled tracer)
      then snd <$> run
      else E.mask $ \restore -> do
        codeAttrs <- callStackAttributes $ qiCallStack qi
        let summary = parseQuerySummary $ qiQuery qi
            renderedSummary = renderQuerySummary summary
            name = if T.null renderedSummary then "postgresql" else renderedSummary
            args =
              defaultSpanArguments
                { kind = Client
                , attributes = H.union (queryAttributes settings qi summary renderedSummary) codeAttrs
                }
        s <- createSpanWithoutCallStack tracer ctx name args
        (outcome, r) <-
          restore run `E.catch` \e -> do
            recordFailure tracer s e
            endSpan s Nothing
            E.throwIO e
        case outcome of
          ReturnedRows n -> addAttribute s "db.response.returned_rows" n
          AffectedRows _ -> pure ()
        endSpan s Nothing
        pure r

queryAttributes :: ObserverSettings -> QueryInfo -> QuerySummary -> T.Text -> AttributeMap
queryAttributes settings qi summary renderedSummary =
  H.fromList $
    catMaybes
      [ Just ("db.system.name", toAttribute @T.Text "postgresql")
      , ("db.namespace",) . toAttribute <$> qiDatabase qi
      , if osRecordQueryText settings
          then Just ("db.query.text", toAttribute $ qiQuery qi)
          else Nothing
      , if T.null renderedSummary
          then Nothing
          else Just ("db.query.summary", toAttribute renderedSummary)
      , -- The semantic conventions forbid extracting db.operation.name and
        -- db.collection.name from the text of the query, so they are not set.
        ("db.stored_procedure.name",) . toAttribute <$> querySummaryStoredProcedure summary
      , ("server.address",) . toAttribute <$> qiHost qi
      , ("server.port",) . toAttribute @Int <$> (readMaybe . T.unpack =<< qiPort qi)
      ]

-- | Attributes of the code that runs the query, i.e. the innermost entry of
-- the call stack.
callStackAttributes :: CallStack -> IO AttributeMap
callStackAttributes cs = case getCallStack cs of
  (_, loc) : rest -> do
    opt <- codeOption <$> getSemanticsOptions
    let function = case rest of
          (caller, _) : _ -> caller
          [] -> "<unknown>"
    pure $ codeAttributes opt function loc
  [] -> pure H.empty

recordFailure :: Tracer -> Span -> E.SomeException -> IO ()
recordFailure tracer s e = do
  let ExceptionResponse classification exAttrs = resolveException tracer e
      exAttrs' = H.insert "exception.escaped" (toAttribute True) exAttrs
  addAttributes s . H.fromList $
    ("error.type", toAttribute errorType)
      : [("db.response.status_code", toAttribute code) | Just code <- [sqlState]]
  -- 'DBException' shows the query with values of its parameters, so only its
  -- error is recorded.
  case E.fromException e of
    Just DBException {dbeError} -> record classification exAttrs' dbeError
    Nothing -> case e of
      E.SomeException inner -> record classification exAttrs' inner
  where
    sqlState = do
      DBException {dbeError} <- E.fromException e
      DetailedQueryError {qeErrorCode} <- cast dbeError
      pure . T.pack $ errorCodeToString qeErrorCode

    -- The SQLSTATE of the error if there is one, the type of the exception
    -- otherwise.
    errorType = fromMaybe (T.pack $ show errorTypeRep) sqlState

    errorTypeRep = case E.fromException e of
      Just DBException {dbeError} -> typeOf dbeError
      Nothing -> case e of
        E.SomeException inner -> typeOf inner

    record :: E.Exception ex => ExceptionClassification -> AttributeMap -> ex -> IO ()
    record classification attrs ex = case classification of
      ErrorException -> do
        setStatus s . Error . T.pack $ E.displayException ex
        recordException s attrs Nothing ex
      RecordedException -> recordException s attrs Nothing ex
      IgnoredException -> pure ()
