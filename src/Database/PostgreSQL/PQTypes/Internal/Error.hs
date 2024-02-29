-- | Definitions of exception types.
module Database.PostgreSQL.PQTypes.Internal.Error
  ( DetailedQueryError (..)
  , QueryError (..)
  , HPQTypesError (..)
  , LibPQError (..)
  , ConversionError (..)
  , ArrayItemError (..)
  , InvalidValue (..)
  , RangeError (..)
  , ArrayDimensionMismatch (..)
  , RowLengthMismatch (..)
  , AffectedRowsMismatch (..)
  ) where

import Control.Exception qualified as E
import Data.Typeable

import Database.PostgreSQL.PQTypes.Internal.Error.Code

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
  deriving (Eq, Ord, Show)

-- | Simple SQL query error. Thrown when there is no
-- PGresult object corresponding to query execution.
newtype QueryError = QueryError String
  deriving (Eq, Ord, Show)

-- | Internal error in this library.
newtype HPQTypesError = HPQTypesError String
  deriving (Eq, Ord)

instance Show HPQTypesError where
  show (HPQTypesError s) = "HPQTypesError (PostgreSQL): " <> s

-- | Internal error in libpq/libpqtypes library.
newtype LibPQError = LibPQError String
  deriving (Eq, Ord)

instance Show LibPQError where
  show (LibPQError s) = "LibPQError (PostgreSQL): " <> s

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

deriving instance Show ConversionError

-- | Array item error. Polymorphic in error type
-- for the same reason as 'ConversionError'.
data ArrayItemError = forall e. E.Exception e => ArrayItemError
  { arrItemIndex :: !Int
  -- ^ Item index (Starts with 1).
  , arrItemError :: !e
  -- ^ Exact error.
  }

deriving instance Show ArrayItemError

-- | \"Invalid value\" error for various data types.
data InvalidValue t = InvalidValue
  { ivValue :: t
  -- ^ Invalid value.
  , -- Optional list of valid values.
    ivValidValues :: Maybe [t]
  }
  deriving (Eq, Ord, Show)

-- | Range error for various data types.
data RangeError t = RangeError
  { reRange :: [(t, t)]
  -- ^ Allowed range (sum of acceptable ranges).
  , reValue :: t
  -- ^ Provided value which is not in above range.
  }
  deriving (Eq, Ord, Show)

-- | Array dimenstion mismatch error.
data ArrayDimensionMismatch = ArrayDimensionMismatch
  { arrDimExpected :: !Int
  -- ^ Dimension expected by the library.
  , arrDimDelivered :: !Int
  -- ^ Dimension provided by the database.
  }
  deriving (Eq, Ord, Show)

-- | Row length mismatch error.
data RowLengthMismatch = RowLengthMismatch
  { lengthExpected :: !Int
  -- ^ Length expected by the library.
  , lengthDelivered :: !Int
  -- ^ Length delivered by the database.
  }
  deriving (Eq, Ord, Show)

-- | Affected/returned rows mismatch error.
data AffectedRowsMismatch = AffectedRowsMismatch
  { rowsExpected :: ![(Int, Int)]
  -- ^ Number of rows expected by the library, expressed as sum of acceptable
  -- ranges, eg. [(1,2), (5,10)] means that it would accept 1, 2, 5, 6, 7, 8,
  -- 9 or 10 affected/returned rows.
  , rowsDelivered :: !Int
  -- ^ Number of affected/returned rows by the database.
  }
  deriving (Eq, Ord, Show)

instance E.Exception DetailedQueryError
instance E.Exception QueryError
instance E.Exception HPQTypesError
instance E.Exception LibPQError
instance E.Exception ConversionError
instance E.Exception ArrayItemError
instance (Show t, Typeable t) => E.Exception (InvalidValue t)
instance (Show t, Typeable t) => E.Exception (RangeError t)
instance E.Exception ArrayDimensionMismatch
instance E.Exception RowLengthMismatch
instance E.Exception AffectedRowsMismatch
