{-# LANGUAGE AllowAmbiguousTypes #-}

module Database.PostgreSQL.PQTypes.Format
  ( PQFormat (..)
  ) where

import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.IP (IP, IPRange)
import Data.Int
import Data.Scientific
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time
import Data.UUID.Types
import Data.Vector qualified as V
import Data.Word

import Database.PostgreSQL.PQTypes.Internal.Oid
import Database.PostgreSQL.PQTypes.Range (Range)

----------------------------------------

-- | Methods in this class are supposed to be used with the
-- @TypeApplications@ extension.
--
-- /Warning:/ the OIDs belong to the type rather than to any of its values, so
-- they don't survive a coercion. If an instance gives a type different OIDs
-- than the one it wraps, 'Database.PostgreSQL.PQTypes.ToSQL.ToSQL' and
-- 'Database.PostgreSQL.PQTypes.FromSQL.FromSQL' cannot be derived from it with
-- @GeneralizedNewtypeDeriving@ (nor @deriving via@ the wrapped type, which
-- amounts to the same thing for a newtype): the derived instances keep the
-- OIDs of the wrapped type, so decoding fails with
-- 'Database.PostgreSQL.PQTypes.Internal.Error.TypeMismatch' and arrays
-- are rejected by the server for having the wrong element type. Write the
-- instances out instead. Deriving them is only safe when the OIDs match,
-- which requires deriving this class along with them.
class PQFormat a where
  -- | OID of the PostgreSQL type corresponding to @a@.
  pqOid :: Oid

  -- | OID of the PostgreSQL array type with elements corresponding to @a@.
  pqArrayOid :: Oid

  -- | 'pqOid' of @[a]@: the array type of @a@ by default, overridden by
  -- 'Char' so that 'String' corresponds to @text@.
  pqListOid :: Oid
  pqListOid = pqArrayOid @a

  -- | 'pqArrayOid' of @[a]@: the same as 'pqArrayOid' by default (an array
  -- is of the same type regardless of its number of dimensions), overridden
  -- by 'Char' so that @[String]@ corresponds to @text[]@.
  pqListArrayOid :: Oid
  pqListArrayOid = pqArrayOid @a

-- NULLables

instance PQFormat a => PQFormat (Maybe a) where
  pqOid = pqOid @a
  pqArrayOid = pqArrayOid @a

-- ARRAYS

-- The OIDs of @[a]@ are routed through class methods so that the 'Char'
-- instance can override them, making 'String' correspond to @text@ instead
-- of an array of single-byte characters.
instance PQFormat a => PQFormat [a] where
  pqOid = pqListOid @a
  pqArrayOid = pqListArrayOid @a

instance PQFormat a => PQFormat (V.Vector a) where
  pqOid = pqArrayOid @a
  pqArrayOid = pqArrayOid @a

-- NUMERICS

instance PQFormat Int16 where
  pqOid = int2Oid
  pqArrayOid = int2ArrayOid

instance PQFormat Int32 where
  pqOid = int4Oid
  pqArrayOid = int4ArrayOid

instance PQFormat Int64 where
  pqOid = int8Oid
  pqArrayOid = int8ArrayOid

instance PQFormat Int where
  pqOid = int8Oid
  pqArrayOid = int8ArrayOid

instance PQFormat Float where
  pqOid = float4Oid
  pqArrayOid = float4ArrayOid

instance PQFormat Double where
  pqOid = float8Oid
  pqArrayOid = float8ArrayOid

instance PQFormat Word16 where
  pqOid = int2Oid
  pqArrayOid = int2ArrayOid

instance PQFormat Word32 where
  pqOid = int4Oid
  pqArrayOid = int4ArrayOid

instance PQFormat Word64 where
  pqOid = int8Oid
  pqArrayOid = int8ArrayOid

instance PQFormat Word where
  pqOid = int8Oid
  pqArrayOid = int8ArrayOid

instance PQFormat Integer where
  pqOid = numericOid
  pqArrayOid = numericArrayOid

instance PQFormat Scientific where
  pqOid = numericOid
  pqArrayOid = numericArrayOid

-- CHAR

instance PQFormat Char where
  pqOid = charOid
  pqArrayOid = charArrayOid
  pqListOid = textOid
  pqListArrayOid = textArrayOid

instance PQFormat Word8 where
  pqOid = charOid
  pqArrayOid = charArrayOid

-- VARIABLE-LENGTH CHARACTER TYPES

instance PQFormat T.Text where
  pqOid = textOid
  pqArrayOid = textArrayOid

instance PQFormat TL.Text where
  pqOid = textOid
  pqArrayOid = textArrayOid

instance PQFormat UUID where
  pqOid = uuidOid
  pqArrayOid = uuidArrayOid

-- BYTEA

instance PQFormat BS.ByteString where
  pqOid = byteaOid
  pqArrayOid = byteaArrayOid

instance PQFormat BSL.ByteString where
  pqOid = byteaOid
  pqArrayOid = byteaArrayOid

-- DATE

instance PQFormat Day where
  pqOid = dateOid
  pqArrayOid = dateArrayOid

-- TIME

instance PQFormat TimeOfDay where
  pqOid = timeOid
  pqArrayOid = timeArrayOid

-- TIMESTAMP

instance PQFormat LocalTime where
  pqOid = timestampOid
  pqArrayOid = timestampArrayOid

-- TIMESTAMPTZ

instance PQFormat UTCTime where
  pqOid = timestamptzOid
  pqArrayOid = timestamptzArrayOid

instance PQFormat ZonedTime where
  pqOid = timestamptzOid
  pqArrayOid = timestamptzArrayOid

-- BOOL

instance PQFormat Bool where
  pqOid = boolOid
  pqArrayOid = boolArrayOid

-- INET / CIDR

instance PQFormat IP where
  pqOid = inetOid
  pqArrayOid = inetArrayOid

instance PQFormat IPRange where
  pqOid = cidrOid
  pqArrayOid = cidrArrayOid

-- RANGES

instance PQFormat (Range Int32) where
  pqOid = int4rangeOid
  pqArrayOid = int4rangeArrayOid

instance PQFormat (Range Int64) where
  pqOid = int8rangeOid
  pqArrayOid = int8rangeArrayOid

instance PQFormat (Range Scientific) where
  pqOid = numrangeOid
  pqArrayOid = numrangeArrayOid

instance PQFormat (Range Day) where
  pqOid = daterangeOid
  pqArrayOid = daterangeArrayOid

instance PQFormat (Range LocalTime) where
  pqOid = tsrangeOid
  pqArrayOid = tsrangeArrayOid

instance PQFormat (Range UTCTime) where
  pqOid = tstzrangeOid
  pqArrayOid = tstzrangeArrayOid
