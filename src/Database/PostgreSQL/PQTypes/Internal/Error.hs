-- | Definitions of exception types.
module Database.PostgreSQL.PQTypes.Internal.Error
  ( DetailedQueryError (..)
  , HPQTypesError (..)
  , LibPQError (..)
  , ConversionError (..)
  , InvalidValue (..)
  , RangeError (..)
  , TypeMismatch (..)
  , ArrayDimensionMismatch (..)
  , RowLengthMismatch (..)
  , AffectedRowsMismatch (..)
  ) where

import Control.Exception qualified as E

import Database.PostgreSQL.PQTypes.Internal.Error.Code
import Database.PostgreSQL.PQTypes.Internal.Oid

-- | SQL query error. Reference: description of PQresultErrorField
-- at <http://www.postgresql.org/docs/devel/static/libpq-exec.html>.
data DetailedQueryError = DetailedQueryError
  { qeSeverity :: !String
  , qeErrorCode :: !ErrorCode
  , qeMessagePrimary :: !String
  , qeMessageDetail :: !(Maybe String)
  , qeMessageHint :: !(Maybe String)
  , qeStatementPosition :: !(Maybe Int)
  , qeInternalPosition :: !(Maybe Int)
  , qeInternalQuery :: !(Maybe String)
  , qeContext :: !(Maybe String)
  , qeSourceFile :: !(Maybe String)
  , qeSourceLine :: !(Maybe Int)
  , qeSourceFunction :: !(Maybe String)
  }
  deriving stock (Eq, Ord, Show)
  deriving anyclass (E.Exception)

-- | Internal error in this library.
newtype HPQTypesError = HPQTypesError String
  deriving stock (Eq, Ord, Show)
  deriving anyclass (E.Exception)

-- | Internal error in libpq library.
newtype LibPQError = LibPQError String
  deriving stock (Eq, Ord, Show)
  deriving anyclass (E.Exception)

-- | Data conversion error. Since it's polymorphic in error type,
-- it nicely reports arbitrarily nested conversion errors.
data ConversionError = forall e. E.Exception e => ConversionError
  { convColumn :: !Int
  -- ^ Column number (Starts with 1).
  , convColumnName :: !String
  -- ^ Name of the column.
  , convRow :: !Int
  -- ^ Row number (Starts with 1).
  , convError :: !e
  -- ^ Exact error.
  }
  deriving anyclass (E.Exception)

deriving stock instance Show ConversionError

-- | \"Invalid value\" error for various data types.
data InvalidValue a = InvalidValue
  { ivValue :: a
  -- ^ Invalid value.
  , -- Optional list of valid values.
    ivValidValues :: Maybe [a]
  }
  deriving stock (Eq, Ord, Show)
  deriving anyclass (E.Exception)

-- | Range error for various data types.
data RangeError a = RangeError
  { reRange :: [(a, a)]
  -- ^ Allowed range (sum of acceptable ranges).
  , reValue :: a
  -- ^ Provided value which is not in above range.
  }
  deriving stock (Eq, Ord, Show)
  deriving anyclass (E.Exception)

-- | Type mismatch error: the type of a field of the query result doesn't
-- match the type expected by the decoder.
data TypeMismatch = TypeMismatch
  { tmExpectedOid :: !Oid
  -- ^ OID of the type expected by the library.
  , tmDeliveredOid :: !Oid
  -- ^ OID of the type delivered by the database.
  }
  deriving stock (Eq, Ord, Show)
  deriving anyclass (E.Exception)

-- | Array dimenstion mismatch error.
data ArrayDimensionMismatch = ArrayDimensionMismatch
  { arrDimExpected :: !Int
  -- ^ Dimension expected by the library.
  , arrDimDelivered :: !Int
  -- ^ Dimension provided by the database.
  }
  deriving stock (Eq, Ord, Show)
  deriving anyclass (E.Exception)

-- | Row length mismatch error.
data RowLengthMismatch = RowLengthMismatch
  { lengthExpected :: !Int
  -- ^ Length expected by the library.
  , lengthDelivered :: !Int
  -- ^ Length delivered by the database.
  }
  deriving stock (Eq, Ord, Show)
  deriving anyclass (E.Exception)

-- | Affected/returned rows mismatch error.
data AffectedRowsMismatch = AffectedRowsMismatch
  { rowsExpected :: ![(Int, Int)]
  -- ^ Number of rows expected by the library, expressed as sum of acceptable
  -- ranges, eg. [(1,2), (5,10)] means that it would accept 1, 2, 5, 6, 7, 8,
  -- 9 or 10 affected/returned rows.
  , rowsDelivered :: !Int
  -- ^ Number of affected/returned rows by the database.
  }
  deriving stock (Eq, Ord, Show)
  deriving anyclass (E.Exception)
