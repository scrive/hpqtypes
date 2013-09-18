{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, TupleSections, TypeFamilies, UndecidableInstances #-}
module Database.PostgreSQL.PQTypes.Internal.Monad where

import Control.Applicative
import Control.Concurrent.MVar
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
import Database.PostgreSQL.PQTypes.Internal.Connection
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

runDBT :: (MonadBaseControl IO m, MonadIO m) => ConnectionSource -> TransactionSettings -> DBT m a -> m a
runDBT cs ts m = withConnection cs $ \mvconn -> do
  evalStateT action $ DBState {
    dbConnection = mvconn
  , dbTransactionSettings = ts
  , dbLastQuery = mempty
  , dbQueryResult = Nothing
  }
  where
    action = unDBT $ if tsAutoTransaction ts
      then do
        let tsNoAuto = ts { tsAutoTransaction = False }
        begin' tsNoAuto
        res <- m `LE.onException` rollback' tsNoAuto
        commit' tsNoAuto
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
    mvconn <- gets (unConnection . dbConnection)
    (affected, res) <- liftIO . modifyMVar mvconn $ \mconn -> case mconn of
      Nothing -> E.throwIO DBException {
        dbeQueryContext = sql
      , dbeError = InternalError "runQuery: no connection"
      }
      Just conn -> E.handle (rethrowWithContext sql) $ do
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
          f nums (SCValue v) = toSQL v conn $ \mbase -> do
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

  getTransactionSettings = DBT . gets $ dbTransactionSettings
  setTransactionSettings ts = DBT . modify $ \st -> st { dbTransactionSettings = ts }

  getQueryResult = DBT $ gets dbQueryResult
  clearQueryResult = DBT . modify $ \st -> st { dbQueryResult = Nothing }

  foldlDB = foldLeft
  foldrDB = foldRight
