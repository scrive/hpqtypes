{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification, StandaloneDeriving #-}
-- | Definitions of exception types.
module Database.PostgreSQL.PQTypes.Internal.Error (
    QueryError(..)
  , InternalError(..)
  , LibPQError(..)
  , ConversionError(..)
  , ArrayItemError(..)
  , RangeError(..)
  , ArrayDimensionMismatch(..)
  , RowLengthMismatch(..)
  , AffectedRowsMismatch(..)
  , throwQueryError
  , throwLibPQError
  , throwLibPQTypesError
  , rethrowWithArrayError
  ) where

import Control.Applicative
import Data.Typeable
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import qualified Control.Exception as E

import Database.PostgreSQL.PQTypes.Internal.C.Interface
import Database.PostgreSQL.PQTypes.Internal.C.Types

-- | SQL query error.
newtype QueryError = QueryError String
  deriving (Eq, Ord, Show, Typeable)

-- | Internal error in this library.
newtype InternalError = InternalError String
  deriving (Eq, Ord, Show, Typeable)

-- | Internal error in libpq/libpqtypes library.
newtype LibPQError = LibPQError String
  deriving (Eq, Ord, Show, Typeable)

-- | Data conversion error. Since it's polymorphic in error type,
-- it nicely reports arbitrarily nested conversion errors.
data ConversionError = forall e. E.Exception e => ConversionError {
-- | Column number (Starts with 1).
  convColumn     :: !Int
-- | Name of the column.
, convColumnName :: !String
-- | Row number (Starts with 1).
, convRow        :: !Int
-- | Exact error.
, convError      :: !e
} deriving Typeable

deriving instance Show ConversionError

-- | Array item error. Polymorphic in error type
-- for the same reason as 'ConversionError'.
data ArrayItemError = forall e. E.Exception e => ArrayItemError {
-- | Item index (Starts with 1).
  arrItemIndex :: !Int
-- | Exact error.
, arrItemError :: !e
} deriving Typeable

deriving instance Show ArrayItemError

-- | Range error for various data types.
data RangeError t = RangeError {
-- | Allowed range (sum of acceptable ranges).
  reRange :: [(t, t)]
-- | Provided value which is not in above range.
, reValue :: t
} deriving (Eq, Ord, Show, Typeable)

-- | Array dimenstion mismatch error.
data ArrayDimensionMismatch = ArrayDimensionMismatch {
-- | Dimension expected by the library.
  arrDimExpected  :: !Int
-- | Dimension provided by the database.
, arrDimDelivered :: !Int
} deriving (Eq, Ord, Show, Typeable)

-- | Row length mismatch error.
data RowLengthMismatch = RowLengthMismatch {
-- | Length expected by the library.
  lengthExpected  :: !Int
-- | Length delivered by the database.
, lengthDelivered :: !Int
} deriving (Eq, Ord, Show, Typeable)

-- | Affected/returned rows mismatch error.
data AffectedRowsMismatch = AffectedRowsMismatch {
-- | Number of rows expected by the library, expressed as sum of
-- acceptable ranges, eg. [(1,2), (5,10)] means that it would
-- accept 1, 2, 5, 6, 7, 8, 9 or 10 affected/returned rows.
  rowsExpected  :: ![(Int, Int)]
-- | Number of affected/returned rows by the database.
, rowsDelivered :: !Int
} deriving (Eq, Ord, Show, Typeable)

instance E.Exception QueryError
instance E.Exception InternalError
instance E.Exception LibPQError
instance E.Exception ConversionError
instance E.Exception ArrayItemError
instance (Show t, Typeable t) => E.Exception (RangeError t)
instance E.Exception ArrayDimensionMismatch
instance E.Exception RowLengthMismatch
instance E.Exception AffectedRowsMismatch

-- | Throw query error.
throwQueryError :: Ptr PGconn -> IO a
throwQueryError conn = do
  msg <- peekCString =<< c_PQerrorMessage conn
  E.throwIO . QueryError $ msg

-- | Throw libpq specific error.
throwLibPQError :: Ptr PGconn -> String -> IO a
throwLibPQError conn ctx = do
  msg <- peekCString =<< c_PQerrorMessage conn
  E.throwIO . LibPQError
    $ if null ctx then msg else ctx ++ ": " ++ msg

-- | Throw libpqtypes specific error.
throwLibPQTypesError :: Ptr PGerror -> String -> IO a
throwLibPQTypesError err ctx = do
  msg <- pgErrorMsg <$> peek err
  E.throwIO . LibPQError
    $ if null ctx then msg else ctx ++ ": " ++ msg

-- | Rethrow supplied exception enriched with array index.
rethrowWithArrayError :: CInt -> E.SomeException -> IO a
rethrowWithArrayError i (E.SomeException e) =
  E.throwIO ArrayItemError {
    arrItemIndex = fromIntegral i + 1
  , arrItemError = e
  }
