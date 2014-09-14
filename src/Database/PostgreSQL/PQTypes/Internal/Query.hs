{-# LANGUAGE FlexibleContexts, RecordWildCards #-}
module Database.PostgreSQL.PQTypes.Internal.Query (
    runSQLQuery
  ) where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad.Base
import Control.Monad.Trans.State
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

-- | Run SQL query.
runSQLQuery :: (IsSQL sql, MonadBase IO m)
            => (StateT DBState m Int -> r) -> sql -> r
runSQLQuery dbT sql = dbT $ do
  mvconn <- gets (unConnection . dbConnection)
  (affected, res) <- liftBase . modifyMVar mvconn $ \mconn -> case mconn of
    Nothing -> E.throwIO DBException {
      dbeQueryContext = sql
    , dbeError = HPQTypesError "runQuery: no connection"
    }
    Just (fconn, stats) ->
      E.handle (rethrowWithContext sql) $ withForeignPtr fconn $ \conn -> do
        (paramCount, res) <- withSQL sql (withPGparam conn) $ \param query -> (,)
          <$> (fromIntegral <$> c_PQparamCount param)
          <*> c_PQparamExec conn nullPtr param query c_RESULT_BINARY
        affected <- withForeignPtr res $ verifyResult conn
        stats' <- case affected of
          Left _ -> return stats {
            statsQueries = statsQueries stats + 1
          , statsParams  = statsParams stats + paramCount
          }
          Right rows -> do
            columns <- fromIntegral <$> withForeignPtr res c_PQnfields
            return stats {
              statsQueries = statsQueries stats + 1
            , statsRows = statsRows stats + rows
            , statsValues = statsValues stats + (rows * columns)
            , statsParams  = statsParams stats + paramCount
            }
        -- Force evaluation of modified stats to squash space leak.
        stats' `seq` return (Just (fconn, stats'), (either id id affected, res))
  modify $ \st -> st {
    dbLastQuery = someSQL sql
  , dbQueryResult = Just $ QueryResult res
  }
  return affected
  where
    verifyResult :: Ptr PGconn -> Ptr PGresult -> IO (Either Int Int)
    verifyResult conn res = do
      -- works even if res is NULL
      st <- c_PQresultStatus res
      case st of
        _ | st == c_PGRES_COMMAND_OK -> do
          sn <- c_PQcmdTuples res >>= BS.packCString
          case BS.readInt sn of
            Nothing
              | BS.null sn -> return . Left $ 0
              | otherwise  -> throwParseError sn
            Just (n, rest)
              | rest /= BS.empty -> throwParseError sn
              | otherwise        -> return . Left $ n
        _ | st == c_PGRES_TUPLES_OK    -> Right . fromIntegral <$> c_PQntuples res
        _ | st == c_PGRES_FATAL_ERROR  -> throwSQLError
        _ | st == c_PGRES_BAD_RESPONSE -> throwSQLError
        _ | otherwise                  -> return . Left $ 0
        where
          throwSQLError = throwQueryError conn res
          throwParseError sn = E.throwIO DBException {
            dbeQueryContext = sql
          , dbeError = HPQTypesError ("runQuery.verifyResult: string returned by PQcmdTuples is not a valid number: " ++ show sn)
          }
