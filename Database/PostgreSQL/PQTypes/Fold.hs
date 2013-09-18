{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
module Database.PostgreSQL.PQTypes.Fold (
    foldLeft
  , foldRight
  ) where

import Control.Monad
import Control.Monad.Trans
import Foreign.ForeignPtr.Safe
import Foreign.C.Types
import qualified Control.Exception as E

import Database.PostgreSQL.PQTypes.Class
import Database.PostgreSQL.PQTypes.Internal.C.Interface
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Exception
import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.Internal.State
import Database.PostgreSQL.PQTypes.Internal.SQL
import Database.PostgreSQL.PQTypes.Internal.Utils
import Database.PostgreSQL.PQTypes.Row

foldLeft :: forall m row acc. (MonadDB m, MonadIO m, Row row)
         => (acc -> row -> m acc) -> acc -> m acc
foldLeft f initAcc = withQueryResult $ \(_::row) ctx res fmt ->
  worker ctx res fmt initAcc 0 =<< liftIO (withForeignPtr res c_PQntuples)
  where
    worker ctx res fmt acc !i !n
      | i == n    = return acc
      | otherwise = do
        obj <- liftIO $ withForeignPtr res $ \pres ->
                        withForeignPtr fmt $ \pfmt ->
                          E.handle (rethrowWithContext ctx) (parseRow pres i pfmt)
        acc' <- f acc obj
        worker ctx res fmt acc' (i+1) n

foldRight :: forall m row acc. (MonadDB m, MonadIO m, Row row)
          => (row -> acc -> m acc) -> acc -> m acc
foldRight f initAcc = withQueryResult $ \(_::row) ctx res fmt ->
  worker ctx res fmt initAcc (-1) . pred =<< liftIO (withForeignPtr res c_PQntuples)
  where
    worker ctx res fmt acc !n !i
      | i == n    = return acc
      | otherwise = do
        obj <- liftIO $ withForeignPtr res $ \pres ->
                        withForeignPtr fmt $ \pfmt ->
                          E.handle (rethrowWithContext ctx) (parseRow pres i pfmt)
        acc' <- f obj acc
        worker ctx res fmt acc' n (i-1)

----------------------------------------

withQueryResult :: forall m row r. (MonadDB m, MonadIO m, Row row)
                => (row -> SQL -> ForeignPtr PGresult -> ForeignPtr CChar -> m r) -> m r
withQueryResult f = do
  mres <- liftM unQueryResult `liftM` getQueryResult
  ctx <- getLastQuery
  case mres of
    Nothing  -> liftIO . E.throwIO $ DBException {
      dbeQueryContext = ctx
    , dbeError = InternalError "withQueryResult: no query result"
    }
    Just res -> do
      liftIO $ do
        rowlen <- fromIntegral `liftM` withForeignPtr res c_PQnfields
        let expected = rowLength (undefined::row)
        when (rowlen /= expected) $
          E.throwIO DBException {
            dbeQueryContext = ctx
          , dbeError = RowLengthMismatch expected rowlen
          }
      fmt <- liftIO . bsToCString $ rowFormat (undefined::row)
      acc <- f (undefined::row) ctx res fmt
      clearQueryResult
      return acc
