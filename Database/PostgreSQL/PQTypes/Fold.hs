{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns, FlexibleContexts, Rank2Types, ScopedTypeVariables #-}
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
import Database.PostgreSQL.PQTypes.Format
import Database.PostgreSQL.PQTypes.FromRow
import Database.PostgreSQL.PQTypes.Internal.C.Interface
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Exception
import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.Internal.State
import Database.PostgreSQL.PQTypes.Internal.Utils
import Database.PostgreSQL.PQTypes.SQL.Class

foldLeftM :: forall m row acc. (MonadBase IO m, MonadDB m, FromRow row)
          => (acc -> row -> m acc) -> acc -> m acc
foldLeftM f initAcc = withQueryResult $ \(_::row) ctx fres ferr ffmt ->
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
foldRightM f initAcc = withQueryResult $ \(_::row) ctx fres ferr ffmt ->
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
                => (forall sql. IsSQL sql => row -> sql -> ForeignPtr PGresult -> ForeignPtr PGerror -> ForeignPtr CChar -> m r) -> m r
withQueryResult f = do
  mres <- getQueryResult
  case mres of
    Nothing -> throwDB . InternalError $ "withQueryResult: no query result"
    Just (QueryResult res) -> do
      SomeSQL ctx <- getLastQuery
      liftBase $ do
        rowlen <- fromIntegral `liftM` withForeignPtr res c_PQnfields
        let expected = pqVariables (undefined::row)
        when (rowlen /= expected) $
          E.throwIO DBException {
            dbeQueryContext = ctx
          , dbeError = RowLengthMismatch expected rowlen
          }
      fmt <- liftBase . bsToCString $ pqFormat (undefined::row)
      err <- liftBase mallocForeignPtr
      acc <- f (undefined::row) ctx res err fmt
      clearQueryResult
      return acc
