{-# LANGUAGE FlexibleContexts, RankNTypes, RecordWildCards
  , TupleSections #-}
module Database.PostgreSQL.PQTypes.Internal.Connection (
    Connection(..)
  , ConnectionData(..)
  , withConnectionData
  , ConnectionStats(..)
  , ConnectionSettings(..)
  , defaultSettings
  , ConnectionSource(..)
  , defaultSource
  , poolSource
  , connect
  , disconnect
  ) where

import Control.Applicative
import Control.Arrow
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Data.Monoid
import Data.Pool
import Data.Time.Clock
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import qualified Control.Exception as E
import qualified Data.ByteString as BS
import qualified Data.Foldable as F

import Database.PostgreSQL.PQTypes.Internal.C.Interface
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Composite
import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.Internal.Exception
import Database.PostgreSQL.PQTypes.Internal.Utils
import Database.PostgreSQL.PQTypes.SQL
import Database.PostgreSQL.PQTypes.SQL.Class

data ConnectionSettings = ConnectionSettings {
-- | Connection info string.
  csConnInfo       :: !BS.ByteString
-- | Client-side encoding. If set to 'Nothing', database encoding is used.
, csClientEncoding :: !(Maybe BS.ByteString)
-- | A list of composite types to register. In order to be able to
-- (de)serialize specific composite types, you need to register them.
, csComposites     :: ![BS.ByteString]
} deriving (Eq, Ord, Show)

-- | Default connection settings.
defaultSettings :: ConnectionSettings
defaultSettings = ConnectionSettings {
  csConnInfo = BS.empty
, csClientEncoding = Nothing
, csComposites = []
}

----------------------------------------

-- | Simple connection statistics.
data ConnectionStats = ConnectionStats {
-- | Number of queries executed so far.
  statsQueries :: !Int
-- | Number of rows fetched from the database.
, statsRows    :: !Int
-- | Number of values fetched from the database.
, statsValues  :: !Int
-- | Number of parameters sent to the database.
, statsParams  :: !Int
} deriving (Eq, Ord, Show)

-- | Initial connection statistics.
initialStats :: ConnectionStats
initialStats = ConnectionStats {
  statsQueries = 0
, statsRows    = 0
, statsValues  = 0
, statsParams  = 0
}

-- | Representation of a connection object.
data ConnectionData = ConnectionData {
  -- | Foreign pointer to pointer to connection object.
  cdFrgnPtr  :: !(ForeignPtr (Ptr PGconn))
  -- | Pointer to connection object (the same as in 'cdFrgnPtr').
, cdPtr      :: !(Ptr PGconn)
  -- | Statistics associated with the connection.
, cdStats    :: !ConnectionStats
}

-- | Wrapper for hiding representation of a connection object.
newtype Connection = Connection {
  unConnection :: MVar (Maybe ConnectionData)
}

withConnectionData :: Connection
                   -> String
                   -> (ConnectionData -> IO (ConnectionData, r))
                   -> IO r
withConnectionData (Connection mvc) fname f =
  modifyMVar mvc $ \mc -> case mc of
    Nothing -> E.throwIO . HPQTypesError $ fname ++ ": no connection"
    Just cd -> first Just <$> f cd

-- | Database connection supplier.
newtype ConnectionSource = ConnectionSource {
  withConnection :: (MonadBase IO m, MonadMask m) => (Connection -> m a) -> m a
}

-- | Default connection supplier. It estabilishes new
-- database connection each time 'withConnection' is called.
defaultSource :: ConnectionSettings -> ConnectionSource
defaultSource cs = ConnectionSource {
  withConnection = bracket (liftBase $ connect cs) (liftBase . disconnect)
}

-- | Pooled source. It uses striped pool from resource-pool
-- package to cache estabilished connections and reuse them.
poolSource :: ConnectionSettings
           -> Int -- ^ Stripe count. The number of distinct sub-pools
           -- to maintain. The smallest acceptable value is 1.
           -> NominalDiffTime -- ^ Amount of time for which an unused database
           -- connection is kept open. The smallest acceptable value is 0.5
           -- seconds.
           --
           -- The elapsed time before closing database connection may be
           -- a little longer than requested, as the reaper thread wakes
           -- at 1-second intervals.
           -> Int -- ^ Maximum number of database connections to keep open
           -- per stripe. The smallest acceptable value is 1.
           --
           -- Requests for database connections will block if this limit is
           -- reached on a single stripe, even if other stripes have idle
           -- connections available.
           -> IO ConnectionSource
poolSource cs numStripes idleTime maxResources = do
  pool <- createPool (connect cs) disconnect numStripes idleTime maxResources
  return ConnectionSource {
    withConnection = withResource' pool . (clearStats >=>)
  }
  where
    withResource' pool m =  mask $ \restore -> do
      (resource, local) <- liftBase $ takeResource pool
      ret <- restore (m resource) `onException`
        liftBase (destroyResource pool local resource)
      liftBase $ putResource local resource
      return ret

    clearStats conn@(Connection mv) = do
      liftBase . modifyMVar_ mv $ \mconn ->
        return $ (\cd -> cd { cdStats = initialStats }) <$> mconn
      return conn

----------------------------------------

-- | Low-level function for connecting to the database.
-- Useful if one wants to implement custom connection source.
connect :: ConnectionSettings -> IO Connection
connect ConnectionSettings{..} = wrapException $ do
  fconn <- BS.useAsCString csConnInfo c_PQconnectdb
  withForeignPtr fconn $ \connPtr -> do
    conn <- peek connPtr
    status <- c_PQstatus conn
    when (status /= c_CONNECTION_OK) $
      throwLibPQError conn "connect"
    F.forM_ csClientEncoding $ \enc -> do
      res <- BS.useAsCString enc (c_PQsetClientEncoding conn)
      when (res == -1) $
        throwLibPQError conn "connect"
    c_PQinitTypes conn
    registerComposites conn csComposites
    Connection <$> newMVar (Just ConnectionData {
      cdFrgnPtr = fconn
    , cdPtr     = conn
    , cdStats   = initialStats
    })

-- | Low-level function for disconnecting from the database.
-- Useful if one wants to implement custom connection source.
disconnect :: Connection -> IO ()
disconnect (Connection mvconn) = wrapException . modifyMVar_ mvconn $ \mconn -> do
  case mconn of
    Just cd -> withForeignPtr (cdFrgnPtr cd) c_PQfinishPtr
    Nothing -> E.throwIO (HPQTypesError "disconnect: no connection (shouldn't happen)")
  return Nothing

----------------------------------------

wrapException :: IO a -> IO a
wrapException = E.handle $ rethrowWithContext (mempty::SQL)
