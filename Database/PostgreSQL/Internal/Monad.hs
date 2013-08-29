{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns, FlexibleContexts, GeneralizedNewtypeDeriving
  , ScopedTypeVariables, TupleSections #-}
module Database.PostgreSQL.Internal.Monad where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Foreign.C.String
import Foreign.ForeignPtr.Safe
import Foreign.Ptr
import qualified Control.Exception as E
import qualified Data.ByteString as BS

import Database.PostgreSQL.Class
import Database.PostgreSQL.Internal.C.Interface
import Database.PostgreSQL.Internal.C.Put
import Database.PostgreSQL.Internal.C.Types
import Database.PostgreSQL.Internal.SQL
import Database.PostgreSQL.Internal.Utils
import Database.PostgreSQL.Row
import Database.PostgreSQL.ToSQL

data DBState = DBState {
  dbConn   :: MVar (Maybe (Ptr PGconn))
, dbResult :: Maybe QueryResult
}

newtype DBT m a = DBT { unDBT :: StateT DBState m a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadIO, MonadTrans)

runDBT :: (MonadBaseControl IO m, MonadIO m) => String -> DBT m a -> m a
runDBT conf m = liftBaseOp (E.bracket connect disconnect) $ \conn -> do
  evalStateT (unDBT m) (DBState conn Nothing)
  where
    connect = do
      conn <- withCString conf c_PQconnectdb
      c_PQinitTypes conn
      newMVar $ Just conn

    disconnect mvconn = modifyMVar_ mvconn $ \mconn -> do
      case mconn of
        Just conn -> c_PQfinish conn
        Nothing   -> error "runDBT.disconnect: no connection (shouldn't happen)"
      return Nothing

foldDB :: forall m base dest acc. (MonadDB m, MonadIO m, Row base dest)
       => (acc -> dest -> m acc) -> acc -> m acc
foldDB f initAcc = do
  mres <- liftM unQueryResult `liftM` queryResult
  case mres of
    Nothing  -> error "foldDB: no query result"
    Just res -> do
      fmt <- liftIO . bsToCString $ rowFormat (undefined::dest)
      liftIO (withForeignPtr res c_PQntuples)
        >>= worker fmt initAcc 0
      where
        worker fmt acc !i !n
          | i == n = return acc
          | otherwise = do
            obj <- liftIO $ withForeignPtr res $ \pres ->
                            withForeignPtr fmt $ \pfmt ->
                              parseRow pres i pfmt
            acc' <- f acc obj
            worker fmt acc' (i+1) n

instance MonadIO m => MonadDB (DBT m) where
  runQuery sql = DBT $ do
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

  queryResult = DBT $ do
    mres <- gets dbResult
    modify $ \st -> st { dbResult = Nothing }
    return mres

  fold = foldDB
