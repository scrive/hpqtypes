{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns, FlexibleContexts, ScopedTypeVariables #-}
module Database.PostgreSQL.PQTypes.Fold (
    foldLeftM
  , foldRightM
  ) where

import Control.Monad
import Control.Monad.Base
import Foreign.ForeignPtr.Safe
import Foreign.C.Types
import qualified Control.Exception as E

import Database.PostgreSQL.PQTypes.Class
import Database.PostgreSQL.PQTypes.FromRow
import Database.PostgreSQL.PQTypes.Internal.C.Interface
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Exception
import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.Internal.State
import Database.PostgreSQL.PQTypes.Internal.SQL
import Database.PostgreSQL.PQTypes.Internal.Utils

foldLeftM :: forall m row acc. (MonadBase IO m, MonadDB m, FromRow row)
          => (acc -> row -> m acc) -> acc -> m acc
foldLeftM f initAcc = withQueryResult $ \(_::row) ctx fres ffmt -> do
  ferr <- liftBase mallocForeignPtr
  liftBase (withForeignPtr fres c_PQntuples)
    >>= worker ctx fres ferr ffmt initAcc 0
  where
    worker ctx fres ferr ffmt acc !i !n
      | i == n    = return acc
      | otherwise = do
        obj <- liftBase $
          withForeignPtr fres $ \res ->
          withForeignPtr ferr $ \err ->
          withForeignPtr ffmt $ \fmt ->
            E.handle (rethrowWithContext ctx) (fromRow res err i fmt)
        acc' <- f acc obj
        worker ctx fres ferr ffmt acc' (i+1) n

foldRightM :: forall m row acc. (MonadBase IO m, MonadDB m, FromRow row)
           => (row -> acc -> m acc) -> acc -> m acc
foldRightM f initAcc = withQueryResult $ \(_::row) ctx fres ffmt -> do
  ferr <- liftBase mallocForeignPtr
  liftBase (withForeignPtr fres c_PQntuples)
    >>= worker ctx fres ferr ffmt initAcc (-1) . pred
  where
    worker ctx fres ferr ffmt acc !n !i
      | i == n    = return acc
      | otherwise = do
        obj <- liftBase $
          withForeignPtr fres $ \res ->
          withForeignPtr ferr $ \err ->
          withForeignPtr ffmt $ \fmt ->
            E.handle (rethrowWithContext ctx) (fromRow res err i fmt)
        acc' <- f obj acc
        worker ctx fres ferr ffmt acc' n (i-1)

----------------------------------------

withQueryResult :: forall m row r. (MonadBase IO m, MonadDB m, FromRow row)
                => (row -> SQL -> ForeignPtr PGresult -> ForeignPtr CChar -> m r) -> m r
withQueryResult f = do
  mres <- liftM unQueryResult `liftM` getQueryResult
  ctx <- getLastQuery
  case mres of
    Nothing  -> liftBase . E.throwIO $ DBException {
      dbeQueryContext = ctx
    , dbeError = InternalError "withQueryResult: no query result"
    }
    Just res -> do
      liftBase $ do
        rowlen <- fromIntegral `liftM` withForeignPtr res c_PQnfields
        let expected = rowLength (undefined::row)
        when (rowlen /= expected) $
          E.throwIO DBException {
            dbeQueryContext = ctx
          , dbeError = RowLengthMismatch expected rowlen
          }
      fmt <- liftBase . bsToCString $ rowFormat (undefined::row)
      acc <- f (undefined::row) ctx res fmt
      clearQueryResult
      return acc
