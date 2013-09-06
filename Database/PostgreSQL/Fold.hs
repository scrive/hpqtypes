{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
module Database.PostgreSQL.Fold where

import Control.Monad
import Control.Monad.Trans
import Foreign.ForeignPtr.Safe
import qualified Control.Exception as E

import Database.PostgreSQL.Class
import Database.PostgreSQL.Internal.C.Interface
import Database.PostgreSQL.Internal.Exception
import Database.PostgreSQL.Internal.Error
import Database.PostgreSQL.Internal.State
import Database.PostgreSQL.Internal.Utils
import Database.PostgreSQL.Row

foldDB :: forall m base dest acc. (MonadDB m, MonadIO m, Row base dest)
       => (acc -> dest -> m acc) -> acc -> m acc
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
        let expected = rowLength (undefined::dest)
        when (rowlen /= expected) $
          E.throwIO DBException {
            dbeQueryContext = ctx
          , dbeError = RowLengthMismatch expected rowlen
          }
      fmt <- liftIO . bsToCString $ rowFormat (undefined::dest)
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
