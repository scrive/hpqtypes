{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, OverlappingInstances
  , UndecidableInstances #-}
-- | Definition of generic 'MonadDB' instance which works for
-- any transformer with 'MonadTrans', 'MonadTransControl'
-- and 'MonadBase' 'IO' instances. Quite convenient, but not
-- imported by default due to OverlappingInstances requirement.
module Database.PostgreSQL.PQTypes.Class.Instances.Overlapping where

import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Database.PostgreSQL.PQTypes.Class
import Database.PostgreSQL.PQTypes.Fold

instance (
    MonadTrans t
  , MonadTransControl t
  , MonadBase IO (t m)
  , MonadDB m
  ) => MonadDB (t m) where
    runQuery = lift . runQuery
    getLastQuery = lift getLastQuery
    getConnectionStats = lift getConnectionStats
    getQueryResult = lift getQueryResult
    clearQueryResult = lift clearQueryResult
    getTransactionSettings = lift getTransactionSettings
    setTransactionSettings = lift . setTransactionSettings
    foldlM = foldLeftM
    foldrM = foldRightM
    throwDB = lift . throwDB
    withNewConnection m = restoreT . return
      =<< liftWith (\run -> withNewConnection (run m))
