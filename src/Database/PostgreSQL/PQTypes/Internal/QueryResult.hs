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

-- | Fold rows of a query result from the right. The fold decodes each row
-- right before it passes it to the fold function, i.e. only after it folded
-- the rows after it. No more rows than the fold function retains are alive at
-- any time.
foldrImpl
  :: (HasCallStack, Monad m)
  => Bool
  -> (t -> acc -> m acc)
  -> acc
  -> QueryResult t
  -> m acc
foldrImpl strict f iacc qr = worker iacc $ checkedNtuples qr - 1
  where
    worker acc i
      | i < 0 = pure acc
      | otherwise = do
          let t = decodeRow qr i
          acc' <- t `seq` f t acc
          worker `apply` acc' $ i - 1

    apply = if strict then ($!) else ($)

-- | Fold rows of a query result from the left. The fold decodes each row
-- right before it passes it to the fold function. No more rows than the fold
-- function retains are alive at any time.
foldlImpl
  :: (HasCallStack, Monad m)
  => Bool
  -> (acc -> t -> m acc)
  -> acc
  -> QueryResult t
  -> m acc
foldlImpl strict f iacc qr = worker iacc 0
  where
    n = checkedNtuples qr
    worker acc i
      | i == n = pure acc
      | otherwise = do
          let t = decodeRow qr i
          acc' <- t `seq` f acc t
          worker `apply` acc' $ i + 1

    apply = if strict then ($!) else ($)

-- | The number of rows of a query result, after a comparison of its width
-- with the row type. A mismatch throws 'RowLengthMismatch' with the query
-- attached as context.
--
-- Both are pure information about the immutable query result, so
-- 'unsafePerformIO' is fine here.
checkedNtuples :: HasCallStack => QueryResult t -> CInt
checkedNtuples (QueryResult (SomeSQL ctx) pid fres g) =
  unsafePerformIO . withForeignPtr fres $ \res -> do
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
    c_PQntuples res
  where
    -- ⊥ of existential type hidden in QueryResult
    row = let _ = g row in row
    rowp = pure row

-- | Decode a row of a query result and attach the query as context to
-- exceptions thrown in the process.
--
-- Decoding is referentially transparent iff the FromRow and FromSQL instances
-- are (the ones provided by the library fulfil this requirement), so
-- 'unsafePerformIO' is fine here.
--
-- The caller must force the result right when it applies the fold function
-- to it. The fold function itself is not obligated to force it, e.g.
-- 'Database.PostgreSQL.PQTypes.Fold.fetchMany' doesn't. Without that,
-- unforced thunks that retain the whole query result escape the fold, and
-- decoding errors surface wherever the thunks are forced.
decodeRow :: HasCallStack => QueryResult t -> CInt -> t
decodeRow (QueryResult (SomeSQL ctx) pid fres g) i =
  unsafePerformIO . withForeignPtr fres $ \res -> alloca $ \err -> do
    -- mask asynchronous exceptions so they won't be wrapped in DBException
    E.mask_ $ (g <$> fromRow res err 0 i) `E.catch` rethrowWithContext ctx pid

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
