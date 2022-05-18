module Database.PostgreSQL.PQTypes.Internal.Query
  ( runQueryIO
  , QueryName(..)
  , runPreparedQueryIO
  ) where

import Control.Concurrent.Async
import Control.Monad
import Data.IORef
import Data.String
import Foreign.ForeignPtr
import Foreign.Ptr
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Set as S

import Database.PostgreSQL.PQTypes.Internal.C.Interface
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Connection
import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.Internal.Error.Code
import Database.PostgreSQL.PQTypes.Internal.Exception
import Database.PostgreSQL.PQTypes.Internal.QueryResult
import Database.PostgreSQL.PQTypes.Internal.State
import Database.PostgreSQL.PQTypes.Internal.Utils
import Database.PostgreSQL.PQTypes.SQL.Class
import Database.PostgreSQL.PQTypes.ToSQL

-- | Low-level function for running an SQL query.
runQueryIO
  :: IsSQL sql
  => sql
  -> DBState m
  -> IO (Int, DBState m)
runQueryIO sql = runQueryImpl "runQueryIO" sql $ \ConnectionData{..} -> do
  let allocParam = ParamAllocator $ withPGparam cdPtr
  withSQL sql allocParam $ \param query -> (,)
    <$> (fromIntegral <$> c_PQparamCount param)
    <*> c_PQparamExec cdPtr nullPtr param query c_RESULT_BINARY

-- | Name of a prepared query.
newtype QueryName = QueryName T.Text
  deriving (Eq, Ord, Show, IsString)

-- | Low-level function for running a prepared SQL query.
runPreparedQueryIO
  :: IsSQL sql
  => QueryName
  -> sql
  -> DBState m
  -> IO (Int, DBState m)
runPreparedQueryIO (QueryName queryName) sql = do
  runQueryImpl "runPreparedQueryIO" sql $ \ConnectionData{..} -> do
    when (T.null queryName) $ do
      E.throwIO DBException
        { dbeQueryContext = sql
        , dbeError = HPQTypesError "runPreparedQueryIO: unnamed prepared query is not supported"
        }
    let allocParam = ParamAllocator $ withPGparam cdPtr
    withSQL sql allocParam $ \param query -> do
      preparedQueries <- readIORef cdPreparedQueries
      BS.useAsCString (T.encodeUtf8 queryName) $ \cname -> do
        when (queryName `S.notMember` preparedQueries) . E.mask_ $ do
          -- Mask asynchronous exceptions, because if preparation of the query
          -- succeeds, we need to reflect that fact in cdPreparedQueries since
          -- you can't prepare a query with the same name more than once.
          res <- c_PQparamPrepare cdPtr nullPtr param cname query
          void . withForeignPtr res $ verifyResult sql cdPtr
          modifyIORef' cdPreparedQueries $ S.insert queryName
        (,) <$> (fromIntegral <$> c_PQparamCount param)
            <*> c_PQparamExecPrepared cdPtr nullPtr param cname c_RESULT_BINARY

----------------------------------------
-- Helpers

-- | Shared implementation of 'runQueryIO' and 'runPreparedQueryIO'.
runQueryImpl
  :: IsSQL sql
  => String
  -> sql
  -> (ConnectionData -> IO (Int, ForeignPtr PGresult))
  -> DBState m
  -> IO (Int, DBState m)
runQueryImpl fname sql execSql st = do
  (affected, res) <- withConnDo $ \cd@ConnectionData{..} -> E.mask $ \restore -> do
    -- While the query runs, the current thread will not be able to receive
    -- asynchronous exceptions. This prevents clients of the library from
    -- interrupting execution of the query. To remedy that we spawn a separate
    -- thread for the query execution and while we wait for its completion, we
    -- are able to receive asynchronous exceptions (assuming that threaded GHC
    -- runtime system is used) and react appropriately.
    queryRunner <- async . restore $ do
      (paramCount, res) <- execSql cd
      affected <- withForeignPtr res $ verifyResult sql cdPtr
      stats' <- case affected of
        Left _ -> return cdStats {
          statsQueries = statsQueries cdStats + 1
        , statsParams  = statsParams cdStats + paramCount
        }
        Right rows -> do
          columns <- fromIntegral <$> withForeignPtr res c_PQnfields
          return ConnectionStats {
            statsQueries = statsQueries cdStats + 1
          , statsRows    = statsRows cdStats + rows
          , statsValues  = statsValues cdStats + (rows * columns)
          , statsParams  = statsParams cdStats + paramCount
          }
      -- Force evaluation of modified stats to squash a space leak.
      stats' `seq` return (cd { cdStats = stats' }, (either id id affected, res))
    -- If we receive an exception while waiting for the execution to complete,
    -- we need to send a request to PostgreSQL for query cancellation and wait
    -- for the query runner thread to terminate. It is paramount we make the
    -- exception handler uninterruptible as we can't exit from the main block
    -- until the query runner thread has terminated.
    E.onException (restore $ wait queryRunner) . E.uninterruptibleMask_ $ do
      c_PQcancel cdPtr >>= \case
        -- If query cancellation request was successfully processed, there is
        -- nothing else to do apart from waiting for the runner to terminate.
        Nothing -> cancel queryRunner
        -- Otherwise we check what happened with the runner. If it already
        -- finished we're fine, just ignore the result. If it didn't, something
        -- weird is going on. Maybe the cancellation request went through when
        -- the thread wasn't making a request to the server? In any case, try to
        -- cancel again and wait for the thread to terminate.
        Just _ -> poll queryRunner >>= \case
          Just _  -> return ()
          Nothing -> do
            void $ c_PQcancel cdPtr
            cancel queryRunner

  return (affected, st {
    dbLastQuery = if dbRecordLastQuery st then SomeSQL sql else dbLastQuery st
  , dbQueryResult = Just QueryResult {
      qrSQL = SomeSQL sql
    , qrResult = res
    , qrFromRow = id
    }
  })
  where
    withConnDo = withConnectionData (dbConnection st) fname

verifyResult :: IsSQL sql => sql -> Ptr PGconn -> Ptr PGresult -> IO (Either Int Int)
verifyResult sql conn res = do
  -- works even if res is NULL
  rst <- c_PQresultStatus res
  case rst of
    _ | rst == c_PGRES_COMMAND_OK -> do
      sn <- c_PQcmdTuples res >>= BS.packCString
      case BS.readInt sn of
        Nothing
          | BS.null sn -> return . Left $ 0
          | otherwise  -> throwParseError sn
        Just (n, rest)
          | rest /= BS.empty -> throwParseError sn
          | otherwise        -> return . Left $ n
    _ | rst == c_PGRES_TUPLES_OK    -> Right . fromIntegral <$> c_PQntuples res
    _ | rst == c_PGRES_FATAL_ERROR  -> throwSQLError
    _ | rst == c_PGRES_BAD_RESPONSE -> throwSQLError
    _ | otherwise                  -> return . Left $ 0
    where
      throwSQLError = rethrowWithContext sql =<< if res == nullPtr
        then return . E.toException . QueryError
          =<< safePeekCString' =<< c_PQerrorMessage conn
        else E.toException <$> (DetailedQueryError
          <$> field c_PG_DIAG_SEVERITY
          <*> (stringToErrorCode <$> field c_PG_DIAG_SQLSTATE)
          <*> field c_PG_DIAG_MESSAGE_PRIMARY
          <*> mfield c_PG_DIAG_MESSAGE_DETAIL
          <*> mfield c_PG_DIAG_MESSAGE_HINT
          <*> ((mread =<<) <$> mfield c_PG_DIAG_STATEMENT_POSITION)
          <*> ((mread =<<) <$> mfield c_PG_DIAG_INTERNAL_POSITION)
          <*> mfield c_PG_DIAG_INTERNAL_QUERY
          <*> mfield c_PG_DIAG_CONTEXT
          <*> mfield c_PG_DIAG_SOURCE_FILE
          <*> ((mread =<<) <$> mfield c_PG_DIAG_SOURCE_LINE)
          <*> mfield c_PG_DIAG_SOURCE_FUNCTION)
        where
          field f = maybe "" id <$> mfield f
          mfield f = safePeekCString =<< c_PQresultErrorField res f

      throwParseError sn = E.throwIO DBException {
        dbeQueryContext = sql
      , dbeError = HPQTypesError ("verifyResult: string returned by PQcmdTuples is not a valid number: " ++ show sn)
      }
