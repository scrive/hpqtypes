module Database.PostgreSQL.PQTypes.Internal.Connection
  ( -- * Connection
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
    -- * Running queries
  , runQueryIO
  , QueryName(..)
  , runPreparedQueryIO
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Data.Bifunctor
import Data.Function
import Data.IORef
import Data.Kind
import Data.Pool
import Data.String
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Ptr
import GHC.Conc (closeFdWith)
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS
import qualified Data.Foldable as F
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Database.PostgreSQL.PQTypes.Internal.C.Interface
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Composite
import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.Internal.Error.Code
import Database.PostgreSQL.PQTypes.Internal.Exception
import Database.PostgreSQL.PQTypes.Internal.Utils
import Database.PostgreSQL.PQTypes.SQL.Class
import Database.PostgreSQL.PQTypes.SQL.Raw
import Database.PostgreSQL.PQTypes.ToSQL

data ConnectionSettings = ConnectionSettings
  { -- | Connection info string.
    csConnInfo       :: !T.Text
    -- | Client-side encoding. If set to 'Nothing', database encoding is used.
  , csClientEncoding :: !(Maybe T.Text)
    -- | A custom role to set with "SET ROLE".
  , csRole           :: !(Maybe (RawSQL ()))
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
  , csRole           = Nothing
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

-- | Pooled source. It uses striped pool from @resource-pool@ package to cache
-- established connections and reuse them.
poolSource
  :: ConnectionSettings
  -> (IO Connection -> (Connection -> IO ()) -> PoolConfig Connection)
  -- ^ A function for creating the 'PoolConfig' with desired parameters.
  --
  -- /Note:/ supplied arguments are for creation and destruction of a database
  -- connection.
  -> IO (ConnectionSource [MonadBase IO, MonadMask])
poolSource cs mkPoolConfig = do
  pool <- newPool $ mkPoolConfig (connect cs) disconnect
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
  connPtr <- BS.useAsCString (T.encodeUtf8 csConnInfo) (openConnection unmask)
  (`onException` c_PQfinish connPtr) . unmask $ do
    status <- c_PQstatus connPtr
    when (status /= c_CONNECTION_OK) $
      throwLibPQError connPtr fname
    F.forM_ csClientEncoding $ \enc -> do
      res <- BS.useAsCString (T.encodeUtf8 enc) (c_PQsetClientEncoding connPtr)
      when (res == -1) $
        throwLibPQError connPtr fname
    c_PQinitTypes connPtr
    registerComposites connPtr csComposites
    conn <- do
      preparedQueries <- newIORef S.empty
      fmap Connection . newMVar $ Just ConnectionData
        { cdPtr = connPtr
        , cdStats = initialStats
        , cdPreparedQueries = preparedQueries
        }
    F.forM_ csRole $ \role -> runQueryIO conn $ "SET ROLE " <> role
    pure conn
  where
    fname = "connect"

    openConnection :: (forall r. IO r -> IO r) -> CString -> IO (Ptr PGconn)
    openConnection unmask conninfo = do
      -- We want to use non-blocking C functions to be able to observe incoming
      -- asynchronous exceptions, hence we don't use PQconnectdb here.
      conn <- c_PQconnectStart conninfo
      when (conn == nullPtr) $
        throwError "PQconnectStart returned a null pointer"
      (`onException` c_PQfinish conn) . unmask $ fix $ \loop -> do
        ps <- c_PQconnectPoll conn
        if | ps == c_PGRES_POLLING_READING -> (threadWaitRead  =<< getFd conn) >> loop
           | ps == c_PGRES_POLLING_WRITING -> (threadWaitWrite =<< getFd conn) >> loop
           | ps == c_PGRES_POLLING_OK      -> return conn
           | otherwise                     -> do
               merr <- c_PQerrorMessage conn >>= safePeekCString
               let reason = maybe "" (\err -> ": " <> err) merr
               throwError $ "openConnection failed" <> reason
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

----------------------------------------
-- Query running

-- | Low-level function for running an SQL query.
runQueryIO
  :: IsSQL sql
  => Connection
  -> sql
  -> IO (Int, ForeignPtr PGresult)
runQueryIO conn sql = do
  runQueryImpl "runQueryIO" conn sql $ \ConnectionData{..} -> do
    let allocParam = ParamAllocator $ withPGparam cdPtr
    withSQL sql allocParam $ \param query -> (,)
      <$> (fromIntegral <$> c_PQparamCount param)
      <*> c_PQparamExec cdPtr nullPtr param query c_RESULT_BINARY

-- | Name of a prepared query.
newtype QueryName = QueryName T.Text
  deriving (Eq, Ord, Show, IsString)

-- | Low-level function for running a prepared SQL query.
runPreparedQueryIO
  :: IsSQL sql
  => Connection
  -> QueryName
  -> sql
  -> IO (Int, ForeignPtr PGresult)
runPreparedQueryIO conn (QueryName queryName) sql = do
  runQueryImpl "runPreparedQueryIO" conn sql $ \ConnectionData{..} -> do
    when (T.null queryName) $ do
      E.throwIO DBException
        { dbeQueryContext = sql
        , dbeError = HPQTypesError "runPreparedQueryIO: unnamed prepared query is not supported"
        }
    let allocParam = ParamAllocator $ withPGparam cdPtr
    withSQL sql allocParam $ \param query -> do
      preparedQueries <- readIORef cdPreparedQueries
      BS.useAsCString (T.encodeUtf8 queryName) $ \cname -> do
        when (queryName `S.notMember` preparedQueries) . E.mask_ $ do
          -- Mask asynchronous exceptions, because if preparation of the query
          -- succeeds, we need to reflect that fact in cdPreparedQueries since
          -- you can't prepare a query with the same name more than once.
          res <- c_PQparamPrepare cdPtr nullPtr param cname query
          void . withForeignPtr res $ verifyResult sql cdPtr
          modifyIORef' cdPreparedQueries $ S.insert queryName
        (,) <$> (fromIntegral <$> c_PQparamCount param)
            <*> c_PQparamExecPrepared cdPtr nullPtr param cname c_RESULT_BINARY

-- | Shared implementation of 'runQueryIO' and 'runPreparedQueryIO'.
runQueryImpl
  :: IsSQL sql
  => String
  -> Connection
  -> sql
  -> (ConnectionData -> IO (Int, ForeignPtr PGresult))
  -> IO (Int, ForeignPtr PGresult)
runQueryImpl fname conn sql execSql = do
  withConnDo $ \cd@ConnectionData{..} -> E.mask $ \restore -> do
    -- While the query runs, the current thread will not be able to receive
    -- asynchronous exceptions. This prevents clients of the library from
    -- interrupting execution of the query. To remedy that we spawn a separate
    -- thread for the query execution and while we wait for its completion, we
    -- are able to receive asynchronous exceptions (assuming that threaded GHC
    -- runtime system is used) and react appropriately.
    queryRunner <- async . restore $ do
      (paramCount, res) <- execSql cd
      affected <- withForeignPtr res $ verifyResult sql cdPtr
      stats' <- case affected of
        Left _ -> return cdStats {
          statsQueries = statsQueries cdStats + 1
        , statsParams  = statsParams cdStats + paramCount
        }
        Right rows -> do
          columns <- fromIntegral <$> withForeignPtr res c_PQnfields
          return ConnectionStats {
            statsQueries = statsQueries cdStats + 1
          , statsRows    = statsRows cdStats + rows
          , statsValues  = statsValues cdStats + (rows * columns)
          , statsParams  = statsParams cdStats + paramCount
          }
      -- Force evaluation of modified stats to squash a space leak.
      stats' `seq` return (cd { cdStats = stats' }, (either id id affected, res))
    -- If we receive an exception while waiting for the execution to complete,
    -- we need to send a request to PostgreSQL for query cancellation and wait
    -- for the query runner thread to terminate. It is paramount we make the
    -- exception handler uninterruptible as we can't exit from the main block
    -- until the query runner thread has terminated.
    E.onException (restore $ wait queryRunner) . E.uninterruptibleMask_ $ do
      c_PQcancel cdPtr >>= \case
        -- If query cancellation request was successfully processed, there is
        -- nothing else to do apart from waiting for the runner to terminate.
        Nothing -> cancel queryRunner
        -- Otherwise we check what happened with the runner. If it already
        -- finished we're fine, just ignore the result. If it didn't, something
        -- weird is going on. Maybe the cancellation request went through when
        -- the thread wasn't making a request to the server? In any case, try to
        -- cancel again and wait for the thread to terminate.
        Just _ -> poll queryRunner >>= \case
          Just _  -> return ()
          Nothing -> do
            void $ c_PQcancel cdPtr
            cancel queryRunner
  where
    withConnDo = withConnectionData conn fname

verifyResult :: IsSQL sql => sql -> Ptr PGconn -> Ptr PGresult -> IO (Either Int Int)
verifyResult sql conn res = do
  -- works even if res is NULL
  rst <- c_PQresultStatus res
  case rst of
    _ | rst == c_PGRES_COMMAND_OK -> do
      sn <- c_PQcmdTuples res >>= BS.packCString
      case BS.readInt sn of
        Nothing
          | BS.null sn -> return . Left $ 0
          | otherwise  -> throwParseError sn
        Just (n, rest)
          | rest /= BS.empty -> throwParseError sn
          | otherwise        -> return . Left $ n
    _ | rst == c_PGRES_TUPLES_OK    -> Right . fromIntegral <$> c_PQntuples res
    _ | rst == c_PGRES_FATAL_ERROR  -> throwSQLError
    _ | rst == c_PGRES_BAD_RESPONSE -> throwSQLError
    _ | otherwise                  -> return . Left $ 0
    where
      throwSQLError = rethrowWithContext sql =<< if res == nullPtr
        then return . E.toException . QueryError
          =<< safePeekCString' =<< c_PQerrorMessage conn
        else E.toException <$> (DetailedQueryError
          <$> field c_PG_DIAG_SEVERITY
          <*> (stringToErrorCode <$> field c_PG_DIAG_SQLSTATE)
          <*> field c_PG_DIAG_MESSAGE_PRIMARY
          <*> mfield c_PG_DIAG_MESSAGE_DETAIL
          <*> mfield c_PG_DIAG_MESSAGE_HINT
          <*> ((mread =<<) <$> mfield c_PG_DIAG_STATEMENT_POSITION)
          <*> ((mread =<<) <$> mfield c_PG_DIAG_INTERNAL_POSITION)
          <*> mfield c_PG_DIAG_INTERNAL_QUERY
          <*> mfield c_PG_DIAG_CONTEXT
          <*> mfield c_PG_DIAG_SOURCE_FILE
          <*> ((mread =<<) <$> mfield c_PG_DIAG_SOURCE_LINE)
          <*> mfield c_PG_DIAG_SOURCE_FUNCTION)
        where
          field f = maybe "" id <$> mfield f
          mfield f = safePeekCString =<< c_PQresultErrorField res f

      throwParseError sn = E.throwIO DBException {
        dbeQueryContext = sql
      , dbeError = HPQTypesError ("verifyResult: string returned by PQcmdTuples is not a valid number: " ++ show sn)
      }
