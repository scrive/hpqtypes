-- | Set of definitions exposed to the end user.
module Database.PostgreSQL.PQTypes (
  -- Database.PostgreSQL.PQTypes.Internal.Connection
  -- * Connection
    Connection
  , ConnectionStats(..)
  , ConnectionSettings(..)
  , ConnectionSource
  , simpleSource
  , poolSource
  -- Database.PostgreSQL.PQTypes.Internal.Error
  -- * Exceptions
  , ErrorCode(..)
  , DetailedQueryError(..)
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
  -- Database.PostgreSQL.PQTypes.Internal.Exception
  , DBException(..)
  -- Database.PostgreSQL.PQTypes.Internal.Monad
  -- * Monad transformer
  , DBT
  , runDBT
  , mapDBT
  -- Database.PostgreSQL.PQTypes.Internal.QueryResult
  -- * Query result
  , QueryResult
  , ntuples
  , nfields
  -- * Other modules
  , module Data.Functor.Identity
  , module Database.PostgreSQL.PQTypes.Array
  , module Database.PostgreSQL.PQTypes.Binary
  , module Database.PostgreSQL.PQTypes.Class
  , module Database.PostgreSQL.PQTypes.Composite
  , module Database.PostgreSQL.PQTypes.Fold
  , module Database.PostgreSQL.PQTypes.Format
  , module Database.PostgreSQL.PQTypes.FromRow
  , module Database.PostgreSQL.PQTypes.FromSQL
  , module Database.PostgreSQL.PQTypes.Interval
  , module Database.PostgreSQL.PQTypes.JSON
  , module Database.PostgreSQL.PQTypes.Notification
  , module Database.PostgreSQL.PQTypes.SQL
  , module Database.PostgreSQL.PQTypes.SQL.Class
  , module Database.PostgreSQL.PQTypes.SQL.Raw
  , module Database.PostgreSQL.PQTypes.ToRow
  , module Database.PostgreSQL.PQTypes.ToSQL
  , module Database.PostgreSQL.PQTypes.Transaction
  , module Database.PostgreSQL.PQTypes.Transaction.Settings
  , module Database.PostgreSQL.PQTypes.Utils
  , module Database.PostgreSQL.PQTypes.XML
  ) where

import Data.Functor.Identity

import Database.PostgreSQL.PQTypes.Internal.Connection
import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.Internal.Error.Code
import Database.PostgreSQL.PQTypes.Internal.Exception
import Database.PostgreSQL.PQTypes.Internal.Monad
import Database.PostgreSQL.PQTypes.Internal.QueryResult

import Database.PostgreSQL.PQTypes.Array
import Database.PostgreSQL.PQTypes.Binary
import Database.PostgreSQL.PQTypes.Class
import Database.PostgreSQL.PQTypes.Composite
import Database.PostgreSQL.PQTypes.Fold
import Database.PostgreSQL.PQTypes.Format
import Database.PostgreSQL.PQTypes.FromRow
import Database.PostgreSQL.PQTypes.FromSQL
import Database.PostgreSQL.PQTypes.Interval
import Database.PostgreSQL.PQTypes.JSON
import Database.PostgreSQL.PQTypes.Notification
import Database.PostgreSQL.PQTypes.SQL
import Database.PostgreSQL.PQTypes.SQL.Class
import Database.PostgreSQL.PQTypes.SQL.Raw
import Database.PostgreSQL.PQTypes.ToRow
import Database.PostgreSQL.PQTypes.ToSQL
import Database.PostgreSQL.PQTypes.Transaction
import Database.PostgreSQL.PQTypes.Transaction.Settings
import Database.PostgreSQL.PQTypes.Utils
import Database.PostgreSQL.PQTypes.XML
