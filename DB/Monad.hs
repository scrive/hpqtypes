{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns, FlexibleContexts, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module DB.Monad where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Foreign.C.String
import Foreign.ForeignPtr.Safe
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import qualified Control.Exception.Lifted as E
import qualified Data.ByteString as BS

import DB.Class
import DB.FromSQL
import DB.Primitive.Interface

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

  fold (f :: acc -> dest -> acc) initAcc = DBT $ do
    mres <- gets dbResult
    case mres of
      Nothing  -> error "No query result"
      Just res -> liftIO $ BS.useAsCString (pqTypesFormat (undefined::dest)) $ \fmt ->
        withForeignPtr res pqNTuples >>= worker fmt initAcc 0
        where
          worker fmt acc !i !n
            | i == n = return acc
            | otherwise = do
              obj <- withForeignPtr res $ \pres -> alloca $ \pObj -> do
                success <- pqGet pres i fmt 0 pObj
                when (success == 0) $ pqGetError >>= peekCString >>= error
                peek pObj
              worker fmt (f acc $ fromSQL obj) (i+1) n
