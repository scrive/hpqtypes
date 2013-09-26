{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts, TupleSections #-}
module Database.PostgreSQL.PQTypes.Internal.Query (
    runSQLQuery
  ) where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad.Base
import Control.Monad.Trans.State
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS

import Database.PostgreSQL.PQTypes.Format
import Database.PostgreSQL.PQTypes.Internal.C.Interface
import Database.PostgreSQL.PQTypes.Internal.C.Put
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Connection
import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.Internal.Exception
import Database.PostgreSQL.PQTypes.Internal.State
import Database.PostgreSQL.PQTypes.Internal.SQL
import Database.PostgreSQL.PQTypes.Internal.Utils
import Database.PostgreSQL.PQTypes.ToSQL

runSQLQuery :: MonadBase IO m => (StateT DBState m Int -> r) -> SQL -> r
runSQLQuery dbT sql = dbT $ do
  mvconn <- gets (unConnection . dbConnection)
  (affected, res) <- liftBase . modifyMVar mvconn $ \mconn -> case mconn of
    Nothing -> E.throwIO DBException {
      dbeQueryContext = sql
    , dbeError = InternalError "runQuery: no connection"
    }
    Just conn -> E.handle (rethrowWithContext sql) $ do
      res <- withPGparam conn $ \param -> do
        query <- loadSQL conn param
        withCString query $ \q -> c_PQparamExec conn nullPtr param q 1
      affected <- withForeignPtr res $ verifyResult conn
      return (mconn, (affected, res))
  modify $ \st -> st {
    dbLastQuery = sql
  , dbQueryResult = Just $ QueryResult res
  }
  return affected
  where
    loadSQL conn param = alloca $ \err -> do
      nums <- newMVar (1::Int)
      concat <$> mapM (f err nums) (unSQL sql)
      where
        f   _    _ (SCString s) = return s
        f err nums (SCValue v) = toSQL v (withPGparam conn) $ \mbase -> do
          BS.useAsCString (pqFormat v) $ \fmt -> do
            verifyPQTRes err "runQuery.loadSQL" =<< c_PQPutfMaybe param err fmt mbase
            modifyMVar nums $ \n -> return . (, "$" ++ show n) $! n+1

    verifyResult conn res
      | res == nullPtr = throwSQLError
      | otherwise = do
        st <- c_PQresultStatus res
        case st of
          _ | st == c_PGRES_COMMAND_OK -> do
            mn <- c_PQcmdTuples res >>= BS.packCString
            case BS.readInt mn of
              Nothing
                | BS.null mn -> return 0
                | otherwise  -> throwParseError
              Just (n, rest)
                | rest /= BS.empty -> throwParseError
                | otherwise        -> return n
          _ | st == c_PGRES_TUPLES_OK    -> fromIntegral <$> c_PQntuples res
          _ | st == c_PGRES_FATAL_ERROR  -> throwSQLError
          _ | st == c_PGRES_BAD_RESPONSE -> throwSQLError
          _ | otherwise                  -> return 0
          where
            throwSQLError = throwQueryError conn
            throwParseError = E.throwIO DBException {
              dbeQueryContext = sql
            , dbeError = InternalError "runQuery.verifyResult: string returned by PQcmdTuples is not a valid number"
            }
