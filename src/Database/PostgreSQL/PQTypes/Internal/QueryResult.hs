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
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
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

mkQueryResult
  :: IsSQL sql
  => sql
  -> BackendPid
  -> ForeignPtr PGresult
  -> QueryResult
mkQueryResult sql pid res =
  QueryResult
    { qrSQL = SomeSQL sql
    , qrBackendPid = pid
    , qrResult = res
    }

foldrImpl
  :: (HasCallStack, Monad m)
  => RowDecoder a
  -> (a -> acc -> m acc)
  -> acc
  -> QueryResult
  -> m acc
foldrImpl dec = foldImpl dec (fmap pred . c_PQntuples) (const . pure $ -1) pred

foldlImpl
  :: (HasCallStack, Monad m)
  => RowDecoder a
  -> (acc -> a -> m acc)
  -> acc
  -> QueryResult
  -> m acc
foldlImpl dec f = foldImpl dec (const $ pure 0) c_PQntuples succ (flip f)

foldImpl
  :: (HasCallStack, Monad m)
  => RowDecoder a
  -> (Ptr PGresult -> IO CInt)
  -> (Ptr PGresult -> IO CInt)
  -> (CInt -> CInt)
  -> (a -> acc -> m acc)
  -> acc
  -> QueryResult
  -> m acc
foldImpl dec initCtr termCtr advCtr f iacc QueryResult {qrSQL = SomeSQL ctx, ..} =
  unsafePerformIO . withForeignPtr qrResult $ \res -> do
    -- This bit is referentially transparent iff the decoder is (the ones
    -- provided by the library fulfil this requirement).
    n <- termCtr res
    let worker acc i
          | i == n = pure acc
          | otherwise = do
              obj <- runDecoder dec res i `E.catch` rethrowWithContext ctx qrBackendPid
              worker (f obj =<< acc) $ advCtr i
    worker (pure iacc) =<< initCtr res

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
