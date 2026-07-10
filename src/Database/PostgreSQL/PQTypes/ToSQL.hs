{-# LANGUAGE AllowAmbiguousTypes #-}

module Database.PostgreSQL.PQTypes.ToSQL
  ( PQParam (..)
  , toPQParam
  , ToSQL (..)
  ) where

import Control.Exception (throw)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.IP (IPRange)
import Data.Int
import Data.List qualified as L
import Data.Scientific
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time
import Data.UUID.Types qualified as U
import Data.Vector qualified as V
import Data.Word
import PostgreSQL.Binary.Encoding qualified as E
import PostgreSQL.Binary.Range (Range)

import Database.PostgreSQL.PQTypes.Format
import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.Internal.Oid

-- | Query parameter in the PostgreSQL binary wire format: OID of its type
-- along with the encoded value ('Nothing' represents SQL NULL).
data PQParam = PQParam !Oid !(Maybe BS.ByteString)
  deriving stock (Eq, Show)

-- | Convert a value to a query parameter.
toPQParam :: forall a. ToSQL a => a -> PQParam
toPQParam v = PQParam (pqOid @a) (E.encodingBytes <$> toSQL v)

-- | Class which represents \"from Haskell type to SQL type\" transformation.
--
-- Time related instances use encoders for servers that have the
-- @integer_datetimes@ parameter set to @on@, which is the case for all
-- supported versions of PostgreSQL (the ability to compile the server with
-- floating point datetimes was removed in PostgreSQL 10).
class PQFormat a => ToSQL a where
  -- | Encode a value in the PostgreSQL binary wire format.
  -- 'Nothing' represents SQL NULL.
  toSQL :: a -> Maybe E.Encoding

  -- | OID of the innermost elements of an array with values of this type as
  -- elements. It coincides with 'pqOid' except for the array wrappers, which
  -- pass through the OID of their own innermost elements (an array is of the
  -- same type regardless of its number of dimensions, so its type is
  -- determined by the innermost elements).
  arrayElemOid :: Oid
  arrayElemOid = pqOid @a

  -- | Encode a value of this type as an element of an array. For most types
  -- this is the encoding of the value itself ('Nothing' becoming a NULL
  -- element), but lists and 'V.Vector's instead become an additional
  -- dimension of the enclosing array.
  arrayElem :: a -> E.Array
  arrayElem = maybe E.nullArray E.encodingArray . toSQL

  -- | 'toSQL' of @[a]@: the encoding of an array by default, overridden by
  -- 'Char' so that 'String' is encoded as @text@. This is why the 'ToSQL'
  -- instance for lists cannot define its methods directly.
  toSQLList :: [a] -> Maybe E.Encoding
  toSQLList = Just . E.array (unOid $ arrayElemOid @a) . arrayElemList

  -- | 'arrayElemOid' of @[a]@: passed through by default (an array is of
  -- the same type regardless of its number of dimensions), overridden by
  -- 'Char' with the OID of @text@.
  arrayElemOidList :: Oid
  arrayElemOidList = arrayElemOid @a

  -- | 'arrayElem' of @[a]@: an additional dimension of the enclosing array
  -- by default, overridden by 'Char' with the encoding of the string as
  -- @text@.
  arrayElemList :: [a] -> E.Array
  arrayElemList xs = case raggedDims xs of
    Nothing -> E.dimensionArray L.foldl' (arrayElem @a) xs
    Just (ds1, ds2) -> raggedArrayError ds1 ds2

  -- | Dimensions of a value of this type as an element of an array:
  -- 'Nothing' for scalars, @Just f@ for lists and 'V.Vector's (which
  -- become an additional dimension of the enclosing array instead of
  -- elements), where @f@ walks down the first branch of the value
  -- collecting lengths:
  --
  -- > f [[1,2,3], [4,5,6]]  =  [2,3]
  -- > f [[1,2,3], [4]]      =  [2,3]
  --
  -- Only the first branch is inspected, so as in the second example the
  -- result describes the other branches only if the value is rectangular;
  -- 'raggedDims' uses it to check exactly that.
  arrayElemDims :: Maybe (a -> [Int])
  arrayElemDims = Nothing

  -- | 'arrayElemDims' of @[a]@: an additional dimension of the enclosing
  -- array by default, overridden by 'Char' ('String' is a scalar @text@
  -- element, so its length is not a dimension).
  arrayElemDimsList :: Maybe ([a] -> [Int])
  arrayElemDimsList = Just $ \xs -> length xs : elemDims xs

  -- The defaults doing per-element work are inlined so that instances in
  -- other modules specialize them instead of calling the generically
  -- compiled defaults (same as 'fromSQLList' in 'FromSQL').
  {-# INLINE arrayElem #-}
  {-# INLINE toSQLList #-}
  {-# INLINE arrayElemList #-}

-- | Check that all elements of an array have the same dimensions
-- ('arrayElemDims'), returning the first differing pair if they don't
-- ('Nothing' right away for scalar elements, which have no dimensions).
--
-- The binary format cannot represent a ragged array: it consists of a
-- single list of dimensions followed by all the elements in one flat
-- sequence. Without this check, @[[1,2], [3]]@ would be sent as a 2x1
-- array (the dimensions are taken from the last sub-array) with 3
-- elements, which the server rejects with an obscure error, while
-- @[[1,2,3], [4], [5,6]]@ would be sent as a 3x2 array with 6 elements,
-- which the server accepts, silently storing @[[1,2], [3,4], [5,6]]@.
--
-- Comparing the first-branch dimensions of the immediate elements is
-- enough, because encoding runs this check at every level of a nested
-- array (see 'arrayElemList' and 'arrayElem' of 'V.Vector'):
--
-- * @[[[1,2], [3,4,5]]]@: the check on the outer array passes (there is
--   only one element to take dimensions of), but encoding that element
--   runs the check on @[[1,2], [3,4,5]]@, which compares @[2]@ with
--   @[3]@ and fails.
--
-- * @[[[1,2]], [[3,4,5]]]@: the checks on the inner arrays pass (one
--   element each), but the check on the outer array compares their
--   dimensions @[1,2]@ with @[1,3]@ and fails.
--
-- In general, if all the checks pass, then at every level every element
-- has the same dimensions as the first branch, i.e. the whole array is
-- rectangular.
raggedDims :: forall a. ToSQL a => [a] -> Maybe ([Int], [Int])
raggedDims xs = case arrayElemDims @a of
  Nothing -> Nothing
  Just f -> case map f xs of
    [] -> Nothing
    ds : rest -> (ds,) <$> L.find (/= ds) rest

-- | Dimensions of the first element of an array ('arrayElemDims'), @[]@ if
-- the array is empty or its elements are scalars.
elemDims :: forall a. ToSQL a => [a] -> [Int]
elemDims xs = case (arrayElemDims @a, xs) of
  (Just f, x : _) -> f x
  _ -> []

raggedArrayError :: [Int] -> [Int] -> r
raggedArrayError ds1 ds2 =
  throw . HPQTypesError $
    "toSQL: multi-dimensional array is ragged: sub-arrays have dimensions "
      ++ show ds1
      ++ " and "
      ++ show ds2

-- NULLables

instance ToSQL a => ToSQL (Maybe a) where
  toSQL ma = toSQL =<< ma
  arrayElemOid = arrayElemOid @a
  arrayElem = maybe E.nullArray (arrayElem @a)
  arrayElemDims = maybe [] <$> arrayElemDims @a

-- ARRAYS

-- The methods of the list instance are routed through class methods so that
-- the 'Char' instance can override them, making 'String' encode as @text@
-- instead of an array of single-byte characters. Nested lists (and
-- 'V.Vector's) merge into a single multi-dimensional array via 'arrayElem',
-- with 'arrayElemOid' passing the OID of the innermost elements through to
-- its top level.
instance ToSQL a => ToSQL [a] where
  toSQL = toSQLList
  arrayElemOid = arrayElemOidList @a
  arrayElem = arrayElemList
  arrayElemDims = arrayElemDimsList @a

instance ToSQL a => ToSQL (V.Vector a) where
  toSQL = Just . E.array (unOid $ arrayElemOid @a) . arrayElem
  arrayElemOid = arrayElemOid @a
  arrayElem v = case raggedDims (V.toList v) of
    Nothing -> E.dimensionArray V.foldl' (arrayElem @a) v
    Just (ds1, ds2) -> raggedArrayError ds1 ds2
  arrayElemDims = Just $ \v -> V.length v : elemDims (V.toList v)

-- NUMERICS

instance ToSQL Int16 where
  toSQL = Just . E.int2_int16

instance ToSQL Int32 where
  toSQL = Just . E.int4_int32

instance ToSQL Int64 where
  toSQL = Just . E.int8_int64

instance ToSQL Int where
  toSQL = Just . E.int8_int64 . fromIntegral

instance ToSQL Float where
  toSQL = Just . E.float4

instance ToSQL Double where
  toSQL = Just . E.float8

instance ToSQL Word16 where
  toSQL = Just . E.int2_word16

instance ToSQL Word32 where
  toSQL = Just . E.int4_word32

instance ToSQL Word64 where
  toSQL = Just . E.int8_word64

instance ToSQL Word where
  toSQL = Just . E.int8_word64 . fromIntegral

instance ToSQL Integer where
  toSQL = Just . E.numeric . fromInteger

instance ToSQL Scientific where
  toSQL = Just . E.numeric

-- CHAR

-- The "char" type stores a single byte, hence characters above '\255' are not
-- representable and the encoding of the rest is the identity, not UTF-8.

instance ToSQL Char where
  toSQL c
    | c > '\255' =
        throw . HPQTypesError $
          "toSQL (Char): character " ++ show c ++ " cannot be losslessly converted to a single byte"
    | otherwise = Just . E.bytea_strict $ BS.singleton c
  toSQLList = Just . E.text_strict . T.pack
  arrayElemOidList = textOid
  arrayElemList = E.encodingArray . E.text_strict . T.pack
  arrayElemDimsList = Nothing

instance ToSQL Word8 where
  toSQL = Just . E.bytea_strict . B.singleton

-- VARIABLE-LENGTH CHARACTER TYPES

instance ToSQL T.Text where
  toSQL = Just . E.text_strict

instance ToSQL TL.Text where
  toSQL = Just . E.text_lazy

instance ToSQL U.UUID where
  toSQL = Just . E.uuid

-- BYTEA

instance ToSQL BS.ByteString where
  toSQL = Just . E.bytea_strict

instance ToSQL BSL.ByteString where
  toSQL = Just . E.bytea_lazy

-- DATE

instance ToSQL Day where
  toSQL = Just . E.date

-- TIME

instance ToSQL TimeOfDay where
  toSQL = Just . E.time_int

-- TIMESTAMP

instance ToSQL LocalTime where
  toSQL = Just . E.timestamp_int

-- TIMESTAMPTZ

instance ToSQL UTCTime where
  toSQL = Just . E.timestamptz_int

instance ToSQL ZonedTime where
  toSQL = Just . E.timestamptz_int . zonedTimeToUTC

-- BOOL

instance ToSQL Bool where
  toSQL = Just . E.bool

-- INET

instance ToSQL IPRange where
  toSQL = Just . E.inet

-- RANGES

instance ToSQL (Range Int32) where
  toSQL = Just . E.int4range

instance ToSQL (Range Int64) where
  toSQL = Just . E.int8range

instance ToSQL (Range Scientific) where
  toSQL = Just . E.numrange

instance ToSQL (Range Day) where
  toSQL = Just . E.daterange

instance ToSQL (Range LocalTime) where
  toSQL = Just . E.tsrange_int

instance ToSQL (Range UTCTime) where
  toSQL = Just . E.tstzrange_int
