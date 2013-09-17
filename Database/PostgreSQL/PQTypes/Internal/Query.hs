{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TupleSections #-}
module Database.PostgreSQL.PQTypes.Internal.Query where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Foreign.C.String
import Foreign.ForeignPtr.Safe
import qualified Control.Exception as E
import qualified Data.ByteString as BS

import Database.PostgreSQL.PQTypes.Class
import Database.PostgreSQL.PQTypes.Internal.C.Interface
import Database.PostgreSQL.PQTypes.Internal.C.Put
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Fold
import Database.PostgreSQL.PQTypes.Internal.Monad
import Database.PostgreSQL.PQTypes.Internal.State
import Database.PostgreSQL.PQTypes.Internal.SQL
import Database.PostgreSQL.PQTypes.Internal.Utils
import Database.PostgreSQL.PQTypes.ToSQL

runDBQuery sql = DBT $ do
  mvconn <- gets dbConn
  res <- liftIO . modifyMVar mvconn $ \mconn -> case mconn of
    Nothing -> error "runQuery: no connection"
    Just conn -> do
      fparam <- pqParamCreate conn
      res <- withForeignPtr fparam $ \param -> do
        query <- loadSQL param
        withCString query $ \q -> pqParamExec conn param q 1
      return (mconn, res)
  modify $ \st -> st { dbResult = Just (QueryResult res) }
  where
    loadSQL param = do
      nums <- newMVar (1::Int)
      concat <$> mapM (f nums) (unSQL sql)
      where
        f _ (SCString s) = return s
        f nums (SCValue v) = toSQL v $ \mbase -> case mbase of
          Nothing -> return "NULL"
          Just (fmt, base) -> BS.useAsCString fmt $ \cfmt -> do
            success <- pqPut param cfmt base
            successCheck success
            modifyMVar nums $ \n -> return . (, "$" ++ show n) $! n+1
