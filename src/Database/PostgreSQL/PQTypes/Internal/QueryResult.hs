{-# LANGUAGE BangPatterns, ExistentialQuantification #-}
module Database.PostgreSQL.PQTypes.Internal.QueryResult (
    QueryResult(..)
  , ntuples
  , nfields
  ) where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import System.IO.Unsafe
import qualified Control.Exception as E

import Database.PostgreSQL.PQTypes.Format
import Database.PostgreSQL.PQTypes.FromRow
import Database.PostgreSQL.PQTypes.Internal.C.Interface
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.Internal.Exception
import Database.PostgreSQL.PQTypes.SQL.Class

-- | Representation of a query result. Provides 'Functor'
-- and 'Foldable' instances for data transformation and
-- extraction appropriately.
data QueryResult t = forall row. FromRow row => QueryResult {
  qrSQL     :: !SomeSQL
, qrResult  :: !(ForeignPtr PGresult)
, qrFromRow :: !(row -> t)
}

instance Functor QueryResult where
  f `fmap` QueryResult ctx fres g = QueryResult ctx fres (f . g)

instance Foldable QueryResult where
  foldr f iacc (QueryResult (SomeSQL ctx) fres g) = unsafePerformIO $ do
    -- This bit is referentially transparent iff appropriate
    -- FrowRow and FromSQL instances are (the ones provided
    -- by the library fulfil this requirement).
    rowlen <- fromIntegral <$> withForeignPtr fres c_PQnfields
    when (rowlen /= pqVariables row) $ E.throwIO DBException {
      dbeQueryContext = ctx
    , dbeError = RowLengthMismatch {
        lengthExpected  = pqVariables row
      , lengthDelivered = rowlen
      }
    }
    alloca $ \err -> withForeignPtr fres $ \res ->
      worker res err iacc . pred =<< c_PQntuples res
    where
      -- ⊥ of existential type hidden in QueryResult
      row = let _ = g row in row

      worker res err !acc !i
        | i == -1   = return acc
        | otherwise = do
          obj <- g <$> fromRow res err 0 i `E.catch` rethrowWithContext ctx
          worker res err (f obj acc) (pred i)

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
