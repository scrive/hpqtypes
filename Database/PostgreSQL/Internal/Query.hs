{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TupleSections #-}
module Database.PostgreSQL.Internal.Query where

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

import Database.PostgreSQL.Class
import Database.PostgreSQL.Internal.C.Interface
import Database.PostgreSQL.Internal.C.Put
import Database.PostgreSQL.Internal.C.Types
import Database.PostgreSQL.Internal.Fold
import Database.PostgreSQL.Internal.Monad
import Database.PostgreSQL.Internal.State
import Database.PostgreSQL.Internal.SQL
import Database.PostgreSQL.Internal.Utils
import Database.PostgreSQL.ToSQL

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
