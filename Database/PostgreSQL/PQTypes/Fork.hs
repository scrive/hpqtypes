{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
module Database.PostgreSQL.PQTypes.Fork where

import Control.Concurrent.Lifted
import Control.Monad.Trans.Control

import Database.PostgreSQL.PQTypes.Class
import Database.PostgreSQL.PQTypes.Internal.Connection
import Database.PostgreSQL.PQTypes.Internal.State
import Database.PostgreSQL.PQTypes.Internal.Transaction

forkWithNewConnection :: (MonadBaseControl IO m, MonadDB m) => m () -> m ThreadId
forkWithNewConnection m = do
  cs <- getConnectionSource
  fork . withConnection cs $ \conn -> do
    clearLastQuery
    clearQueryResult
    getTransactionSettings
      >>= localConnection (const conn) . action
  where
    action ts = if tsAutoTransaction ts
      then withTransaction' (ts { tsAutoTransaction = False }) m
      else m
