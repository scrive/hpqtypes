{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts, Rank2Types, RecordWildCards #-}
module Database.PostgreSQL.PQTypes.Internal.Connection (
    Connection(..)
  , ConnectionStats(..)
  , ConnectionSettings(..)
  , defaultSettings
  , ConnectionSource(..)
  , defaultSource
  , poolSource
  ) where

import Control.Applicative
import Control.Arrow
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.Monoid
import Data.Pool
import Data.Time.Clock
import Foreign.ForeignPtr
import qualified Control.Exception as E
import qualified Data.ByteString as BS
import qualified Data.Foldable as F

import Database.PostgreSQL.PQTypes.Internal.C.Interface
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Composite
import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.Internal.Exception
import Database.PostgreSQL.PQTypes.SQL

data ConnectionSettings = ConnectionSettings {
  csConnInfo       :: !BS.ByteString
, csClientEncoding :: !(Maybe BS.ByteString)
, csComposites     :: ![BS.ByteString]
} deriving (Eq, Ord, Show)

defaultSettings :: ConnectionSettings
defaultSettings = ConnectionSettings {
  csConnInfo = BS.empty
, csClientEncoding = Nothing
, csComposites = []
}

----------------------------------------

data ConnectionStats = ConnectionStats {
  statsQueries :: !Int
, statsRows    :: !Int
, statsValues  :: !Int
, statsParams  :: !Int
} deriving (Eq, Ord, Show)

emptyStats :: ConnectionStats
emptyStats = ConnectionStats {
  statsQueries = 0
, statsRows    = 0
, statsValues  = 0
, statsParams  = 0
}

newtype Connection = Connection {
  unConnection :: MVar (Maybe (ForeignPtr PGconn, ConnectionStats))
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
    withConnection = withResource pool . (clearStats >=>)
  }
  where
    clearStats conn@(Connection mv) = do
      liftBase . modifyMVar_ mv $ \mconn ->
        return $ second (const emptyStats) <$> mconn
      return conn

----------------------------------------

connect :: ConnectionSettings -> IO Connection
connect ConnectionSettings{..} = wrapException $ do
  fconn <- BS.useAsCString csConnInfo c_PQconnectdb
  withForeignPtr fconn $ \conn -> do
    status <- c_PQstatus conn
    when (status /= c_CONNECTION_OK) $
      throwLibPQError conn "connect"
    F.forM_ csClientEncoding $ \enc -> do
      res <- BS.useAsCString enc (c_PQsetClientEncoding conn)
      when (res == -1) $
        throwLibPQError conn "connect"
    c_PQinitTypes conn
    registerComposites conn csComposites
  Connection <$> newMVar (Just (fconn, emptyStats))

disconnect :: Connection -> IO ()
disconnect (Connection mvconn) = wrapException . modifyMVar_ mvconn $ \mconn -> do
  case mconn of
    Just (conn, _) -> finalizeForeignPtr conn
    Nothing -> E.throwIO (InternalError "disconnect: no connection (shouldn't happen)")
  return Nothing

wrapException :: IO a -> IO a
wrapException = E.handle $ rethrowWithContext (mempty::SQL)
