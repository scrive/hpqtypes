{-# LANGUAGE FlexibleContexts, RecordWildCards #-}
module Database.PostgreSQL.PQTypes.Internal.Query (
    runQueryIO
  ) where

import Control.Applicative
import Foreign.ForeignPtr
import Foreign.Ptr
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS

import Database.PostgreSQL.PQTypes.Internal.C.Interface
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Connection
import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.Internal.Exception
import Database.PostgreSQL.PQTypes.Internal.QueryResult
import Database.PostgreSQL.PQTypes.Internal.State
import Database.PostgreSQL.PQTypes.SQL.Class
import Database.PostgreSQL.PQTypes.Internal.Utils

-- | Low-level function for running SQL query.
runQueryIO :: IsSQL sql => sql -> DBState -> IO (Int, DBState)
runQueryIO sql st = E.handle (rethrowWithContext sql) $ do
  (affected, res) <- withConnectionData (dbConnection st) "runQueryIO" $ \cd -> do
    let ConnectionData{..} = cd
    (paramCount, res) <- withSQL sql (withPGparam cdPtr) $ \param query -> (,)
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
    dbLastQuery = someSQL sql
  , dbQueryResult = Just $ QueryResult res
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
          throwSQLError = throwQueryError conn res
          throwParseError sn = E.throwIO DBException {
            dbeQueryContext = sql
          , dbeError = HPQTypesError ("runQuery.verifyResult: string returned by PQcmdTuples is not a valid number: " ++ show sn)
          }
