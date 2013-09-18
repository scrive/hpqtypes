{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts, Rank2Types, RecordWildCards #-}
module Database.PostgreSQL.PQTypes.Internal.Connection (
    ConnectionSettings(..)
  , Connection(..)
  , ConnectionSource(..)
  , defaultSource
  , poolSource
  ) where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Trans.Control
import Data.Pool
import Data.Time.Clock
import Foreign.Ptr
import Foreign.C.String
import qualified Control.Exception as E

import Database.PostgreSQL.PQTypes.Internal.C.Interface
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Composite
import Database.PostgreSQL.PQTypes.Internal.Error

data ConnectionSettings = ConnectionSettings {
  csConnInfo   :: String
, csComposites :: [String]
}

----------------------------------------

newtype Connection = Connection {
  unConnection :: MVar (Maybe (Ptr PGconn))
}

newtype ConnectionSource = ConnectionSource {
  withConnection :: MonadBaseControl IO m => (Connection -> m a) -> m a
}

defaultSource :: ConnectionSettings -> ConnectionSource
defaultSource cs = ConnectionSource {
  withConnection = liftBaseOp $ E.bracket (connect cs) disconnect
}

poolSource :: ConnectionSettings -> Int -> NominalDiffTime -> Int -> IO ConnectionSource
poolSource cs numStripes idleTime maxResources = do
  pool <- createPool (connect cs) disconnect numStripes idleTime maxResources
  return ConnectionSource {
    withConnection = withResource pool
  }

----------------------------------------

connect :: ConnectionSettings -> IO Connection
connect ConnectionSettings{..} = do
  conn <- withCString csConnInfo c_PQconnectdb
  status <- c_PQstatus conn
  when (status /= c_CONNECTION_OK) $
    throwLibPQError conn "connect"
  c_PQinitTypes conn
  registerComposites conn csComposites
  Connection <$> newMVar (Just conn)

disconnect :: Connection -> IO ()
disconnect (Connection mvconn) = modifyMVar_ mvconn $ \mconn -> do
  case mconn of
    Just conn -> c_PQfinish conn
    Nothing   -> E.throwIO $ InternalError "disconnect: no connection (shouldn't happen)"
  return Nothing
