module Database.PostgreSQL.PQTypes.Internal.QueryResult
  ( QueryResult (..)
  , ntuples
  , nfields

    -- * Implementation
  , foldrImpl
  , foldlImpl
  ) where

import Control.Exception qualified as E
import Foreign.C.Types
import Foreign.ForeignPtr
import GHC.Stack
import System.IO.Unsafe

import Database.PostgreSQL.PQTypes.Internal.BackendPid
import Database.PostgreSQL.PQTypes.Internal.C.Interface
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Exception
import Database.PostgreSQL.PQTypes.Internal.RowDecoder
import Database.PostgreSQL.PQTypes.SQL.Class

-- | Representation of a query result. Its rows can be converted to Haskell
-- values with the fetching functions from "Database.PostgreSQL.PQTypes.Fold",
-- which take a 'RowDecoder' of a row as an argument.
data QueryResult = QueryResult
  { qrSQL :: !SomeSQL
  , qrBackendPid :: !BackendPid
  , qrResult :: !(ForeignPtr PGresult)
  }

-- | Fold rows of a query result from the right. Each row is decoded right
-- before it's passed to the fold function, i.e. only after the rows after it
-- were folded, so no more rows than the fold function retains are alive at
-- any time.
foldrImpl
  :: (HasCallStack, Monad m)
  => RowDecoder a
  -> (a -> acc -> m acc)
  -> acc
  -> QueryResult
  -> m acc
foldrImpl dec f iacc qr = worker iacc $ n - 1
  where
    n = fromIntegral $ ntuples qr
    worker acc !i
      | i < 0 = pure acc
      | otherwise = do
          let a = decodeRow dec qr i
          acc' <- a `seq` f a acc
          worker acc' (i - 1)

-- | Fold rows of a query result from the left. Each row is decoded right
-- before it's passed to the fold function, so no more rows than the fold
-- function retains are alive at any time.
foldlImpl
  :: (HasCallStack, Monad m)
  => RowDecoder a
  -> (acc -> a -> m acc)
  -> acc
  -> QueryResult
  -> m acc
foldlImpl dec f iacc qr = worker iacc 0
  where
    n = fromIntegral $ ntuples qr
    worker acc !i
      | i == n = pure acc
      | otherwise = do
          let a = decodeRow dec qr i
          acc' <- a `seq` f acc a
          worker acc' (i + 1)

-- | Decode a row of a query result, attaching the query as context to
-- exceptions thrown while doing so.
--
-- The restricted interface of 'RowDecoder' only allows reading fields of the
-- (immutable) query result, so decoding is referentially transparent and
-- duplicating it is harmless, which makes the usage of 'unsafeDupablePerformIO'
-- fine here.
--
-- Note that the result needs to be forced right when the fold function is
-- applied to it (the fold function itself is not obligated to force it,
-- consider e.g. 'fetchMany'), otherwise unforced thunks retaining the whole
-- query result would escape the fold and decoding errors would be deferred
-- to wherever they're forced.
decodeRow :: HasCallStack => RowDecoder a -> QueryResult -> CInt -> a
decodeRow dec QueryResult {qrSQL = SomeSQL ctx, ..} i =
  unsafeDupablePerformIO . withForeignPtr qrResult $ \res ->
    runDecoder dec res i `E.catch` rethrowWithContext ctx qrBackendPid

-- Note: c_PQntuples/c_PQnfields are pure on a C level and QueryResult
-- constructor is not exported to the end user (so it's not possible
-- to enforce premature finalization via finalizeForeignPtr), which
-- makes usage of unsafeDupablePerformIO fine here.

-- | Extract number of returned tuples (rows) from query result.
ntuples :: QueryResult -> Int
ntuples qr = unsafeDupablePerformIO $ do
  fromIntegral <$> withForeignPtr (qrResult qr) c_PQntuples

-- | Extract number of returned fields (columns) from query result.
nfields :: QueryResult -> Int
nfields qr = unsafeDupablePerformIO $ do
  fromIntegral <$> withForeignPtr (qrResult qr) c_PQnfields
