module Database.PostgreSQL.PQTypes.Internal.QueryResult
  ( QueryResult (..)
  , mkQueryResult
  , ntuples
  , nfields

    -- * Implementation
  , foldrImpl
  , foldlImpl
  ) where

import Control.Exception qualified as E
import Control.Monad
import Data.Coerce
import Data.Foldable
import Data.Functor.Identity
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import GHC.Stack
import System.IO.Unsafe

import Database.PostgreSQL.PQTypes.Format
import Database.PostgreSQL.PQTypes.FromRow
import Database.PostgreSQL.PQTypes.Internal.BackendPid
import Database.PostgreSQL.PQTypes.Internal.C.Interface
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.Internal.Exception
import Database.PostgreSQL.PQTypes.SQL.Class

-- | Representation of a query result. Provides 'Functor'
-- and 'Foldable' instances for data transformation and
-- extraction appropriately.
data QueryResult t = forall row. FromRow row => QueryResult
  { qrSQL :: !SomeSQL
  , qrBackendPid :: !BackendPid
  , qrResult :: !(ForeignPtr PGresult)
  , qrFromRow :: !(row -> t)
  }

mkQueryResult
  :: (FromRow t, IsSQL sql)
  => sql
  -> BackendPid
  -> ForeignPtr PGresult
  -> QueryResult t
mkQueryResult sql pid res =
  QueryResult
    { qrSQL = SomeSQL sql
    , qrBackendPid = pid
    , qrResult = res
    , qrFromRow = id
    }

instance Functor QueryResult where
  f `fmap` QueryResult ctx pid fres g = QueryResult ctx pid fres (f . g)

instance Foldable QueryResult where
  foldr f acc = runIdentity . foldrImpl False (coerce f) acc
  foldr' f acc = runIdentity . foldrImpl True (coerce f) acc

  foldl f acc = runIdentity . foldlImpl False (coerce f) acc
  foldl' f acc = runIdentity . foldlImpl True (coerce f) acc

foldrImpl
  :: (HasCallStack, Monad m)
  => Bool
  -> (t -> acc -> m acc)
  -> acc
  -> QueryResult t
  -> m acc
foldrImpl = foldImpl (fmap pred . c_PQntuples) (const . pure $ -1) pred

foldlImpl
  :: (HasCallStack, Monad m)
  => Bool
  -> (acc -> t -> m acc)
  -> acc
  -> QueryResult t
  -> m acc
foldlImpl strict = foldImpl (const $ pure 0) c_PQntuples succ strict . flip

foldImpl
  :: (HasCallStack, Monad m)
  => (Ptr PGresult -> IO CInt)
  -> (Ptr PGresult -> IO CInt)
  -> (CInt -> CInt)
  -> Bool
  -> (t -> acc -> m acc)
  -> acc
  -> QueryResult t
  -> m acc
foldImpl initCtr termCtr advCtr strict f iacc (QueryResult (SomeSQL ctx) pid fres g) =
  unsafePerformIO . withForeignPtr fres $ \res -> do
    -- This bit is referentially transparent iff appropriate
    -- FrowRow and FromSQL instances are (the ones provided
    -- by the library fulfil this requirement).
    rowlen <- fromIntegral <$> c_PQnfields res
    when (rowlen /= pqVariablesP rowp) $
      E.throwIO
        DBException
          { dbeQueryContext = ctx
          , dbeBackendPid = pid
          , dbeError =
              RowLengthMismatch
                { lengthExpected = pqVariablesP rowp
                , lengthDelivered = rowlen
                }
          , dbeCallStack = callStack
          }
    alloca $ \err -> do
      n <- termCtr res
      let worker acc i =
            if i == n
              then pure acc
              else do
                -- mask asynchronous exceptions so they won't be wrapped in DBException
                obj <- E.mask_ (g <$> fromRow res err 0 i `E.catch` rethrowWithContext ctx pid)
                worker `apply` (f obj =<< acc) $ advCtr i
      worker (pure iacc) =<< initCtr res
  where
    -- ⊥ of existential type hidden in QueryResult
    row = let _ = g row in row
    rowp = pure row

    apply = if strict then ($!) else ($)

-- Note: c_PQntuples/c_PQnfields are pure on a C level and QueryResult
-- constructor is not exported to the end user (so it's not possible
-- to enforce premature finalization via finalizeForeignPtr), which
-- makes usage of unsafeDupablePerformIO fine here.

-- | Extract number of returned tuples (rows) from query result.
ntuples :: QueryResult t -> Int
ntuples qr = unsafeDupablePerformIO $ do
  fromIntegral <$> withForeignPtr (qrResult qr) c_PQntuples

-- | Extract number of returned fields (columns) from query result.
nfields :: QueryResult t -> Int
nfields qr = unsafeDupablePerformIO $ do
  fromIntegral <$> withForeignPtr (qrResult qr) c_PQnfields
