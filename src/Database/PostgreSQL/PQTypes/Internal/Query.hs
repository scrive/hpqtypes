module Database.PostgreSQL.PQTypes.Internal.Query (
    runQueryIO
  ) where

import Control.Applicative
import Foreign.ForeignPtr
import Foreign.Ptr
import Prelude
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS

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

-- | Low-level function for running SQL query.
runQueryIO :: IsSQL sql => sql -> DBState m -> IO (Int, DBState m)
runQueryIO sql st = do
  (affected, res) <- withConnectionData (dbConnection st) "runQueryIO" $ \cd -> do
    let ConnectionData{..} = cd
        allocParam = ParamAllocator $ withPGparam cdPtr
    (paramCount, res) <- withSQL sql allocParam $ \param query -> (,)
      <$> (fromIntegral <$> c_PQparamCount param)
      <*> c_PQparamExec cdPtr nullPtr param query c_RESULT_BINARY
    affected <- withForeignPtr res $ verifyResult cdPtr
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
    -- Force evaluation of modified stats to squash space leak.
    stats' `seq` return (cd { cdStats = stats' }, (either id id affected, res))
  return (affected, st {
    dbLastQuery = SomeSQL sql
  , dbQueryResult = Just QueryResult {
      qrSQL = SomeSQL sql
    , qrResult = res
    , qrFromRow = id
    }
  })
  where
    verifyResult :: Ptr PGconn -> Ptr PGresult -> IO (Either Int Int)
    verifyResult conn res = do
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
          , dbeError = HPQTypesError ("runQuery.verifyResult: string returned by PQcmdTuples is not a valid number: " ++ show sn)
          }
