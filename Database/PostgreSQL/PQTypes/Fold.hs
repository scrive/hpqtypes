{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
module Database.PostgreSQL.PQTypes.Fold where

import Control.Monad
import Control.Monad.Trans
import Foreign.ForeignPtr.Safe
import qualified Control.Exception as E

import Database.PostgreSQL.PQTypes.Class
import Database.PostgreSQL.PQTypes.Internal.C.Interface
import Database.PostgreSQL.PQTypes.Internal.Exception
import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.Internal.State
import Database.PostgreSQL.PQTypes.Internal.Utils
import Database.PostgreSQL.PQTypes.Row

foldDB :: forall m row acc. (MonadDB m, MonadIO m, Row row)
       => (acc -> row -> m acc) -> acc -> m acc
foldDB f initAcc = do
  mres <- liftM unQueryResult `liftM` getQueryResult
  ctx <- getLastQuery
  case mres of
    Nothing  -> liftIO . E.throwIO $ DBException {
      dbeQueryContext = ctx
    , dbeError = InternalError "foldDB: no query result"
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
      acc <- liftIO (withForeignPtr res c_PQntuples)
        >>= worker fmt initAcc 0
      clearQueryResult
      return acc
      where
        worker fmt acc !i !n
          | i == n    = return acc
          | otherwise = do
            obj <- liftIO $ withForeignPtr res $ \pres ->
                            withForeignPtr fmt $ \pfmt ->
                              E.handle (addContext ctx) (parseRow pres i pfmt)
            acc' <- f acc obj
            worker fmt acc' (i+1) n
