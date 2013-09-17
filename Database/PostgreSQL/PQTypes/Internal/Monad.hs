{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, TupleSections, TypeFamilies, UndecidableInstances #-}
module Database.PostgreSQL.PQTypes.Internal.Monad where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.Monoid
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Ptr
import qualified Control.Exception as E
import qualified Control.Exception.Lifted as LE
import qualified Data.ByteString.Char8 as BS

import Database.PostgreSQL.PQTypes.Class
import Database.PostgreSQL.PQTypes.Fold
import Database.PostgreSQL.PQTypes.Internal.C.Interface
import Database.PostgreSQL.PQTypes.Internal.C.Put
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Composite
import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.Internal.Exception
import Database.PostgreSQL.PQTypes.Internal.Format
import Database.PostgreSQL.PQTypes.Internal.State
import Database.PostgreSQL.PQTypes.Internal.SQL
import Database.PostgreSQL.PQTypes.Internal.Transaction
import Database.PostgreSQL.PQTypes.Internal.Utils
import Database.PostgreSQL.PQTypes.ToSQL

type InnerDBT = StateT DBState

newtype DBT m a = DBT { unDBT :: InnerDBT m a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadIO, MonadTrans)

runDBT :: (MonadBaseControl IO m, MonadIO m) => String -> TransactionMode -> DBT m a -> m a
runDBT conf tm m = liftBaseOp (E.bracket connect disconnect) $ \mvconn -> do
  evalStateT action $ DBState {
    dbConnection = mvconn
  , dbTransactionMode = tm
  , dbLastQuery = mempty
  , dbQueryResult = Nothing
  }
  where
    connect = do
      conn <- withCString conf c_PQconnectdb
      status <- c_PQstatus conn
      when (status /= c_CONNECTION_OK) $
        throwLibPQError conn "runDBT.connect"
      c_PQinitTypes conn
      registerComposites conn ["simple", "nested"]
      newMVar $ Just conn

    disconnect mvconn = modifyMVar_ mvconn $ \mconn -> do
      case mconn of
        Just conn -> c_PQfinish conn
        Nothing   -> E.throwIO $ InternalError "runDBT.disconnect: no connection (shouldn't happen)"
      return Nothing

    action = unDBT $
      if tmAutoTransaction tm
        then do
          let tmNoAuto = tm { tmAutoTransaction = False }
          begin' tmNoAuto
          res <- m `LE.onException` rollback' tmNoAuto
          commit' tmNoAuto
          return res
        else m

instance MonadTransControl DBT where
  newtype StT DBT a = StDBT { unStDBT :: StT InnerDBT a }
  liftWith = defaultLiftWith DBT unDBT StDBT
  restoreT = defaultRestoreT DBT unStDBT
  {-# INLINE liftWith #-}
  {-# INLINE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (DBT m) where
  newtype StM (DBT m) a = StMDBT { unStMDBT :: ComposeSt DBT m a }
  liftBaseWith = defaultLiftBaseWith StMDBT
  restoreM     = defaultRestoreM unStMDBT
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

instance MonadIO m => MonadDB (DBT m) where
  runQuery sql = DBT $ do
    mvconn <- gets dbConnection
    (affected, res) <- liftIO . modifyMVar mvconn $ \mconn -> case mconn of
      Nothing -> E.throwIO DBException {
        dbeQueryContext = sql
      , dbeError = InternalError "runQuery: no connection"
      }
      Just conn -> E.handle (addContext sql) $ do
        res <- withPGparam conn $ \param -> do
          query <- loadSQL conn param
          withCString query $ \q -> c_safePQparamExec conn param q 1
        affected <- withForeignPtr res $ verifyResult conn
        return (mconn, (affected, res))
    modify $ \st -> st {
      dbLastQuery = sql
    , dbQueryResult = Just $ QueryResult res
    }
    return affected
    where
      loadSQL conn param = do
        nums <- newMVar (1::Int)
        concat <$> mapM (f nums) (unSQL sql)
        where
          f _ (SCString s) = return s
          f nums (SCValue v) = toSQL conn v $ \mbase -> do
            BS.useAsCString (pqFormat v) $ \fmt -> do
              verifyPQTRes "runQuery.loadSQL" =<< c_PQPutfMaybe param fmt mbase
              modifyMVar nums $ \n -> return . (, "$" ++ show n) $! n+1

      verifyResult conn res
        | res == nullPtr = throwPQError
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
            _ | st == c_PGRES_FATAL_ERROR  -> throwPQError
            _ | st == c_PGRES_BAD_RESPONSE -> throwPQError
            _ | otherwise                  -> return 0
            where
              throwPQError = throwLibPQError conn "runQuery.verifyResult"
              throwParseError = E.throwIO DBException {
                dbeQueryContext = sql
              , dbeError = InternalError "runQuery.verifyResult: string returned by PQcmdTuples is not a valid number"
              }

  getLastQuery = DBT . gets $ dbLastQuery

  getTransactionMode = DBT . gets $ dbTransactionMode
  setTransactionMode tm = DBT . modify $ \st -> st { dbTransactionMode = tm }

  getQueryResult = DBT $ gets dbQueryResult
  clearQueryResult = DBT . modify $ \st -> st { dbQueryResult = Nothing }

  fold = foldDB
