{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns, FlexibleContexts, GeneralizedNewtypeDeriving
  , ScopedTypeVariables, TupleSections #-}
module DB.Monad where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.ForeignPtr.Safe
import Foreign.Ptr
import qualified Control.Exception as E

import DB.Class
import DB.Primitive.Interface
import DB.Primitive.Types
import DB.Row

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
      conn <- withCString conf pqConnectDb
      pqInitTypes conn
      newMVar $ Just conn

    disconnect mvconn = modifyMVar_ mvconn $ \mconn -> do
      case mconn of
        Just conn -> pqFinish conn
        Nothing   -> error "runDBT.disconnect: no connection (shouldn't happen)"
      return Nothing

foldDB :: forall m base dest acc. (MonadDB m, MonadIO m, Row base dest)
       => (acc -> dest -> m acc) -> acc -> m acc
foldDB f initAcc = do
  mres <- liftM unQueryResult `liftM` queryResult
  case mres of
    Nothing  -> error "foldDB: no query result"
    Just res -> do
      fmt <- liftIO . E.mask_ $ newForeignPtr finalizerFree =<< newCString rowFmt
      liftIO (withForeignPtr res $ return . pqNTuples)
        >>= worker fmt initAcc 0
      where
        rowFmt = rowFormat (undefined::dest)
        worker fmt acc !i !n
          | i == n = return acc
          | otherwise = do
            obj <- liftIO $ withForeignPtr res $ \pres ->
                            withForeignPtr fmt $ \pfmt ->
                              parseRow pres i pfmt
            acc' <- f acc obj
            worker fmt acc' (i+1) n

instance MonadIO m => MonadDB (DBT m) where
  runQuery query = DBT $ do
    mvconn <- gets dbConn
    res <- liftIO . modifyMVar mvconn $ \mconn -> case mconn of
      Nothing -> error "runQuery: no connection"
      Just conn -> (mconn, ) `liftM` E.mask_ (newForeignPtr pqClear
        =<< withCString query (\q -> pqParamExec conn nullPtr q 1))
    modify $ \st -> st { dbResult = Just (QueryResult res) }

  queryResult = DBT $ do
    mres <- gets dbResult
    modify $ \st -> st { dbResult = Nothing }
    return mres

  fold = foldDB
