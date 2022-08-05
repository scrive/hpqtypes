module Database.PostgreSQL.PQTypes.Internal.Connection (
    Connection(..)
  , ConnectionData(..)
  , withConnectionData
  , ConnectionStats(..)
  , ConnectionSettings(..)
  , defaultConnectionSettings
  , ConnectionSourceM(..)
  , ConnectionSource(..)
  , simpleSource
  , poolSource
  , connect
  , disconnect
  ) where

import Control.Arrow (first)
import Control.Concurrent
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Data.Function
import Data.IORef
import Data.Kind
import Data.Pool
import Data.Time.Clock
import Foreign.C.String
import Foreign.Ptr
import GHC.Conc (closeFdWith)
import qualified Control.Exception as E
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Database.PostgreSQL.PQTypes.Internal.C.Interface
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Composite
import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.Internal.Utils

data ConnectionSettings = ConnectionSettings
  { -- | Connection info string.
    csConnInfo       :: !T.Text
    -- | Client-side encoding. If set to 'Nothing', database encoding is used.
  , csClientEncoding :: !(Maybe T.Text)
    -- | A list of composite types to register. In order to be able to
    -- (de)serialize specific composite types, you need to register them.
  , csComposites     :: ![T.Text]
  } deriving (Eq, Ord, Show)

-- | Default connection settings. Note that all strings sent to PostgreSQL by
-- the library are encoded as UTF-8, so don't alter client encoding unless you
-- know what you're doing.
defaultConnectionSettings :: ConnectionSettings
defaultConnectionSettings =
  ConnectionSettings
  { csConnInfo       = T.empty
  , csClientEncoding = Just "UTF-8"
  , csComposites     = []
  }

----------------------------------------

-- | Simple connection statistics.
data ConnectionStats = ConnectionStats
  { -- | Number of queries executed so far.
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
--
-- /Note:/ PGconn is not managed with a ForeignPtr because finalizers are broken
-- and at program exit might run even though another thread is inside the
-- relevant withForeignPtr block, executing a safe FFI call (in this case
-- executing an SQL query).
--
-- See https://gitlab.haskell.org/ghc/ghc/-/issues/10975 for more info.
data ConnectionData = ConnectionData
  { cdPtr      :: !(Ptr PGconn)
  -- ^ Pointer to connection object.
  , cdStats    :: !ConnectionStats
  -- ^ Statistics associated with the connection.
  , cdPreparedQueries :: !(IORef (S.Set T.Text))
  -- ^ A set of named prepared statements of the connection.
  }

-- | Wrapper for hiding representation of a connection object.
newtype Connection = Connection {
  unConnection :: MVar (Maybe ConnectionData)
}

withConnectionData
  :: Connection
  -> String
  -> (ConnectionData -> IO (ConnectionData, r))
  -> IO r
withConnectionData (Connection mvc) fname f =
  modifyMVar mvc $ \mc -> case mc of
    Nothing -> hpqTypesError $ fname ++ ": no connection"
    Just cd -> first Just <$> f cd

-- | Database connection supplier.
newtype ConnectionSourceM m = ConnectionSourceM {
  withConnection :: forall r. (Connection -> m r) -> m r
}

-- | Wrapper for a polymorphic connection source.
newtype ConnectionSource (cs :: [(Type -> Type) -> Constraint]) = ConnectionSource {
  unConnectionSource :: forall m. MkConstraint m cs => ConnectionSourceM m
}

-- | Default connection supplier. It establishes new
-- database connection each time 'withConnection' is called.
simpleSource
  :: ConnectionSettings
  -> ConnectionSource [MonadBase IO, MonadMask]
simpleSource cs = ConnectionSource $ ConnectionSourceM {
  withConnection = bracket (liftBase $ connect cs) (liftBase . disconnect)
}

-- | Pooled source. It uses striped pool from resource-pool
-- package to cache established connections and reuse them.
poolSource
  :: ConnectionSettings
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
  -> IO (ConnectionSource [MonadBase IO, MonadMask])
poolSource cs numStripes idleTime maxResources = do
  pool <- createPool (connect cs) disconnect numStripes idleTime maxResources
  return $ ConnectionSource $ ConnectionSourceM {
    withConnection = doWithConnection pool . (clearStats >=>)
  }
  where
    doWithConnection pool m = fst <$> generalBracket
      (liftBase $ takeResource pool)
      (\(resource, local) -> \case
          ExitCaseSuccess _ -> liftBase $ putResource local resource
          _                 -> liftBase $ destroyResource pool local resource
      )
      (\(resource, _) -> m resource)

    clearStats conn@(Connection mv) = do
      liftBase . modifyMVar_ mv $ \mconn ->
        return $ (\cd -> cd { cdStats = initialStats }) <$> mconn
      return conn

----------------------------------------

-- | Low-level function for connecting to the database. Useful if one wants to
-- implement custom connection source.
--
-- /Warning:/ the 'Connection' needs to be explicitly destroyed with
-- 'disconnect', otherwise there will be a resource leak.
connect :: ConnectionSettings -> IO Connection
connect ConnectionSettings{..} = mask $ \unmask -> do
  conn <- BS.useAsCString (T.encodeUtf8 csConnInfo) (openConnection unmask)
  (`onException` c_PQfinish conn) . unmask $ do
    status <- c_PQstatus conn
    when (status /= c_CONNECTION_OK) $
      throwLibPQError conn fname
    F.forM_ csClientEncoding $ \enc -> do
      res <- BS.useAsCString (T.encodeUtf8 enc) (c_PQsetClientEncoding conn)
      when (res == -1) $
        throwLibPQError conn fname
    c_PQinitTypes conn
    registerComposites conn csComposites
    preparedQueries <- newIORef S.empty
    fmap Connection . newMVar $ Just ConnectionData
      { cdPtr = conn
      , cdStats = initialStats
      , cdPreparedQueries = preparedQueries
      }
  where
    fname = "connect"

    openConnection :: (forall r. IO r -> IO r) -> CString -> IO (Ptr PGconn)
    openConnection unmask conninfo = do
      -- We want to use non-blocking C functions to be able to observe incoming
      -- asynchronous exceptions, hence we don't use PQconnectdb here.
      conn <- c_PQconnectStart conninfo
      when (conn == nullPtr) $
        throwError "PQconnectStart returned a null pointer"
      fd <- getFd conn
      (`onException` c_PQfinish conn) . unmask $ fix $ \loop -> do
        ps <- c_PQconnectPoll conn
        if | ps == c_PGRES_POLLING_READING -> threadWaitRead  fd >> loop
           | ps == c_PGRES_POLLING_WRITING -> threadWaitWrite fd >> loop
           | ps == c_PGRES_POLLING_OK      -> return conn
           | otherwise                     -> throwError "openConnection failed"
      where
        getFd conn = do
          fd <- c_PQsocket conn
          when (fd == -1) $
            throwError "invalid file descriptor"
          return fd

        throwError :: String -> IO a
        throwError = hpqTypesError . (fname ++) . (": " ++)

-- | Low-level function for disconnecting from the database. Useful if one wants
-- to implement custom connection source.
disconnect :: Connection -> IO ()
disconnect (Connection mvconn) = modifyMVar_ mvconn $ \mconn -> do
  case mconn of
    Just cd -> do
      let conn = cdPtr cd
      -- This covers the case when a connection is closed while other Haskell
      -- threads are using GHC's IO manager to wait on the descriptor. This is
      -- commonly the case with asynchronous notifications, for example. Since
      -- libpq is responsible for opening and closing the file descriptor, GHC's
      -- IO manager needs to be informed that the file descriptor has been
      -- closed. The IO manager will then raise an exception in those threads.
      c_PQsocket conn >>= \case
        -1 -> c_PQfinish conn -- can happen if the connection is bad/lost
        fd -> closeFdWith (\_ -> c_PQfinish conn) fd

    Nothing -> E.throwIO (HPQTypesError "disconnect: no connection (shouldn't happen)")
  return Nothing
