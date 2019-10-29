-- | Definitions of exception types.
module Database.PostgreSQL.PQTypes.Internal.Error (
    DetailedQueryError(..)
  , QueryError(..)
  , HPQTypesError(..)
  , LibPQError(..)
  , ConversionError(..)
  , ArrayItemError(..)
  , InvalidValue(..)
  , RangeError(..)
  , ArrayDimensionMismatch(..)
  , RowLengthMismatch(..)
  , AffectedRowsMismatch(..)
  ) where

import Data.Typeable
import qualified Control.Exception as E

import Database.PostgreSQL.PQTypes.Internal.Error.Code

-- | SQL query error. Reference: description of PQresultErrorField
-- at <http://www.postgresql.org/docs/devel/static/libpq-exec.html>.
data DetailedQueryError = DetailedQueryError
  { qeSeverity          :: !String
  , qeErrorCode         :: !ErrorCode
  , qeMessagePrimary    :: !String
  , qeMessageDetail     :: !(Maybe String)
  , qeMessageHint       :: !(Maybe String)
  , qeStatementPosition :: !(Maybe Int)
  , qeInternalPosition  :: !(Maybe Int)
  , qeInternalQuery     :: !(Maybe String)
  , qeContext           :: !(Maybe String)
  , qeSourceFile        :: !(Maybe String)
  , qeSourceLine        :: !(Maybe Int)
  , qeSourceFunction    :: !(Maybe String)
  } deriving (Eq, Ord, Show)

-- | Simple SQL query error. Thrown when there is no
-- PGresult object corresponding to query execution.
newtype QueryError = QueryError String
  deriving (Eq, Ord, Show)

-- | Internal error in this library.
newtype HPQTypesError = HPQTypesError String
  deriving (Eq, Ord, Show)

-- | Internal error in libpq/libpqtypes library.
newtype LibPQError = LibPQError String
  deriving (Eq, Ord, Show)

-- | Data conversion error. Since it's polymorphic in error type,
-- it nicely reports arbitrarily nested conversion errors.
data ConversionError = forall e. E.Exception e => ConversionError
  { -- | Column number (Starts with 1).
    convColumn     :: !Int
    -- | Name of the column.
  , convColumnName :: !String
    -- | Row number (Starts with 1).
  , convRow        :: !Int
    -- | Exact error.
  , convError      :: !e
  }

deriving instance Show ConversionError

-- | Array item error. Polymorphic in error type
-- for the same reason as 'ConversionError'.
data ArrayItemError = forall e. E.Exception e => ArrayItemError
  { -- | Item index (Starts with 1).
    arrItemIndex :: !Int
    -- | Exact error.
  , arrItemError :: !e
}

deriving instance Show ArrayItemError

-- | \"Invalid value\" error for various data types.
data InvalidValue t = InvalidValue
  { -- | Invalid value.
    ivValue       :: t
    -- Optional list of valid values.
  , ivValidValues :: Maybe [t]
  } deriving (Eq, Ord, Show)

-- | Range error for various data types.
data RangeError t = RangeError
  { -- | Allowed range (sum of acceptable ranges).
    reRange :: [(t, t)]
    -- | Provided value which is not in above range.
  , reValue :: t
  } deriving (Eq, Ord, Show)

-- | Array dimenstion mismatch error.
data ArrayDimensionMismatch = ArrayDimensionMismatch
  { -- | Dimension expected by the library.
    arrDimExpected  :: !Int
    -- | Dimension provided by the database.
  , arrDimDelivered :: !Int
  } deriving (Eq, Ord, Show)

-- | Row length mismatch error.
data RowLengthMismatch = RowLengthMismatch
  { -- | Length expected by the library.
    lengthExpected  :: !Int
    -- | Length delivered by the database.
  , lengthDelivered :: !Int
  } deriving (Eq, Ord, Show)

-- | Affected/returned rows mismatch error.
data AffectedRowsMismatch = AffectedRowsMismatch
  { -- | Number of rows expected by the library, expressed as sum of acceptable
    -- ranges, eg. [(1,2), (5,10)] means that it would accept 1, 2, 5, 6, 7, 8,
    -- 9 or 10 affected/returned rows.
    rowsExpected  :: ![(Int, Int)]
    -- | Number of affected/returned rows by the database.
  , rowsDelivered :: !Int
  } deriving (Eq, Ord, Show)

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
