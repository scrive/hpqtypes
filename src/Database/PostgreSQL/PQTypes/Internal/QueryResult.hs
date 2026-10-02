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

-- | Representation of a query result. The fetching functions from
-- "Database.PostgreSQL.PQTypes.Fold" convert its rows to Haskell values. They
-- take a 'RowDecoder' of a row as an argument.
data QueryResult = QueryResult
  { qrSQL :: !SomeSQL
  , qrBackendPid :: !BackendPid
  , qrResult :: !(ForeignPtr PGresult)
  }

-- | Fold rows of a query result from the right. The fold decodes each row
-- right before it passes it to the fold function, i.e. only after it folded
-- the rows after it. No more rows than the fold function retains are alive at
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

-- | Fold rows of a query result from the left. The fold decodes each row
-- right before it passes it to the fold function. No more rows than the fold
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

-- | Decode a row of a query result and attach the query as context to
-- exceptions thrown in the process.
--
-- The restricted interface of 'RowDecoder' only allows reading fields of the
-- immutable query result. Decoding is thus referentially transparent, and a
-- duplicate run of it is harmless, so 'unsafeDupablePerformIO' is fine here.
--
-- The caller must force the result right when it applies the fold function
-- to it. The fold function itself is not obligated to force it, e.g.
-- 'Database.PostgreSQL.PQTypes.Fold.fetchMany' doesn't. Without that,
-- unforced thunks that retain the whole query result escape the fold, and
-- decoding errors surface wherever the thunks are forced.
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
