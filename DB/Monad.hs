{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns, FlexibleContexts, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module DB.Monad where

import Control.Applicative
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Foreign.ForeignPtr.Safe
import Foreign.Ptr
import qualified Control.Exception.Lifted as E
import qualified Data.ByteString as BS

import DB.Class
import DB.Primitive.Interface
import DB.Primitive.Types
import DB.Row

data DBState = DBState {
    dbConn     :: Ptr PGconn
  , dbResult   :: Maybe (ForeignPtr PGresult)
}

newtype DBT m a = DBT { unDBT :: StateT DBState m a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadIO, MonadTrans)

runDBT :: (MonadBaseControl IO m, MonadIO m) => BS.ByteString -> DBT m a -> m a
runDBT conf m = E.bracket (liftIO connect) (liftIO . pqFinish)
  $ \conn -> evalStateT (unDBT m) (DBState conn Nothing)
  where
    connect = do
      conn <- BS.useAsCString conf pqConnectDb
      pqInitTypes conn
      return conn

instance MonadIO m => MonadDB (DBT m) where
  run query = DBT $ do
    conn <- gets dbConn
    res <- liftIO $ newForeignPtr pqClear
      =<< BS.useAsCString query (\q -> pqParamExec conn nullPtr q 1)
    modify $ \st -> st { dbResult = Just res }

  fold (f :: acc -> dest -> IO acc) initAcc = DBT $ do
    mres <- gets dbResult
    case mres of
      Nothing  -> error "No query result"
      Just res -> liftIO $ BS.useAsCString (rowFormat dest) $ \fmt ->
        withForeignPtr res (return . pqNTuples) >>= worker fmt initAcc 0
        where
          dest = undefined :: dest
          worker fmt acc !i !n
            | i == n = return acc
            | otherwise = do
              acc' <- f acc =<< withForeignPtr res (parseRow fmt i)
              worker fmt acc' (i+1) n
