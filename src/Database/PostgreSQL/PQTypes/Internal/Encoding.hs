-- | Encoders of the PostgreSQL binary wire format.
--
-- Derived from the @postgresql-binary@ package by Nikita Volkov (MIT
-- licensed, see @LICENSE@), reduced to the types this library supports and
-- adjusted where its behavior didn't suit us. The divergences, each also
-- noted at its definition below:
--
-- * 'text_strict' and 'text_lazy' encode the value verbatim. Upstream
--   silently drops NUL characters from it, which corrupts the value instead
--   of letting the server reject it (the server does so with @22021
--   character_not_in_repertoire@).
--
-- * 'float4' and 'float8' obtain the bit pattern with the casts from
--   "GHC.Float". Upstream coerces the value, which is undefined behavior and
--   produces the wrong bytes on a big-endian architecture.
--
-- * 'numeric' rejects a value whose weight, scale or number of digit groups
--   doesn't fit the 16 bit fields of the header. Upstream lets them wrap
--   around, which silently encodes an entirely different number (@1e131072@
--   used to arrive as @0@).
--
-- * The time encoders take the values apart with the accessors of the
--   "Data.Time" and "Data.Fixed" types instead of coercing them to their
--   representations.
--
-- * 'inet' takes the address and the length of its netmask, leaving it to
--   the caller to map a Haskell type onto that. Upstream takes an
--   'IP.IPRange', which cannot carry the host bits of the address.
--
-- * t'Encoding' is a newtype rather than an alias of the underlying builder,
--   and 'array' takes an t'Oid' rather than a bare 'Word32', so
--   that neither the builder library nor a raw OID leaks into the public
--   API of the library.
module Database.PostgreSQL.PQTypes.Internal.Encoding
  ( -- * Encoding
    Encoding
  , encodingBytes

    -- * Arrays
  , Array
  , array
  , encodingArray
  , nullArray
  , dimensionArray

    -- * Numbers
  , int2_int16
  , int2_word16
  , int4_int32
  , int4_word32
  , int8_int64
  , int8_word64
  , float4
  , float8
  , numeric

    -- * Character types
  , text_strict
  , text_lazy

    -- * Byte arrays
  , bytea_strict
  , bytea_lazy

    -- * Date and time
  , date
  , time_int
  , timestamp_int
  , timestamptz_int

    -- * Miscellaneous
  , bool
  , uuid
  , inet

    -- * JSON
  , json_bytes
  , json_bytes_lazy
  , json_ast
  , jsonb_bytes
  , jsonb_bytes_lazy
  , jsonb_ast

    -- * Ranges
  , int4range
  , int8range
  , numrange
  , daterange
  , tsrange_int
  , tstzrange_int
  ) where

import ByteString.StrictBuilder qualified as B
import Control.Exception (throw)
import Data.Aeson qualified as A
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Fixed (Fixed (..))
import Data.IP qualified as IP
import Data.Int
import Data.List qualified as L
import Data.Scientific qualified as S
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Time
import Data.UUID.Types qualified as U
import Data.Word
import GHC.Float (castDoubleToWord64, castFloatToWord32)

import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.Internal.Oid
import Database.PostgreSQL.PQTypes.Range

-- | An encoded value, i.e. its payload in the PostgreSQL binary wire format
-- (without the length prefix, which is supplied out of band).
newtype Encoding = Encoding B.Builder
  deriving newtype (Monoid, Semigroup)

-- | Render an t'Encoding' as the bytes to be sent to the server.
encodingBytes :: Encoding -> BS.ByteString
encodingBytes (Encoding builder) = B.builderBytes builder

----------------------------------------
-- Helpers

-- | Prefix a payload with its length, as the elements of arrays and the
-- bounds of ranges are represented on the wire.
sized :: Encoding -> Encoding
sized (Encoding payload) =
  Encoding $ B.int32BE (fromIntegral $ B.builderLength payload) <> payload

-- | The length prefix representing a NULL element.
null4 :: Encoding
null4 = int4_int32 (-1)

----------------------------------------
-- Arrays

-- | An array under construction: its payload, the sizes of its dimensions
-- and whether any of its elements is NULL.
data Array = Array !Encoding ![Int32] !Bool

-- | Turn an t'Array' into a value, given the OID of the type of its
-- innermost elements.
array :: Oid -> Array -> Encoding
array elemOid (Array payload dims nulls) =
  mconcat
    [ int4_int32 . fromIntegral $ length dims
    , int4_word32 $ if nulls then 1 else 0
    , int4_word32 $ unOid elemOid
    , foldMap dimension dims
    , payload
    ]
  where
    -- Every dimension is sent with a lower bound of 1.
    dimension n = int4_int32 n <> int4_word32 1

-- | A single element of an array.
encodingArray :: Encoding -> Array
encodingArray value = Array (sized value) [] False

-- | A NULL element of an array.
nullArray :: Array
nullArray = Array null4 [] True

-- | An additional dimension of an array, given a way to fold the container
-- holding its elements.
dimensionArray :: (forall b. (b -> a -> b) -> b -> c -> b) -> (a -> Array) -> c -> Array
dimensionArray foldlElems elemArray input = Array payload (len : dims) nulls
  where
    (payload, dims, len, nulls) = foldlElems step (mempty, [], 0, False) input

    step (!accPayload, _, !accLen, !accNulls) x =
      case elemArray x of
        Array elemPayload elemDims elemNulls ->
          ( accPayload <> elemPayload
          , elemDims
          , accLen + 1
          , accNulls || elemNulls
          )

----------------------------------------
-- Numbers

int2_int16 :: Int16 -> Encoding
int2_int16 = Encoding . B.int16BE

int2_word16 :: Word16 -> Encoding
int2_word16 = Encoding . B.word16BE

int4_int32 :: Int32 -> Encoding
int4_int32 = Encoding . B.int32BE

int4_word32 :: Word32 -> Encoding
int4_word32 = Encoding . B.word32BE

int8_int64 :: Int64 -> Encoding
int8_int64 = Encoding . B.int64BE

int8_word64 :: Word64 -> Encoding
int8_word64 = Encoding . B.word64BE

-- Note: the bit patterns are obtained with the casts from "GHC.Float" rather
-- than by coercing the values, which is undefined behavior (it reads the
-- payload of a boxed value and assumes a little-endian layout).

float4 :: Float -> Encoding
float4 = int4_word32 . castFloatToWord32

float8 :: Double -> Encoding
float8 = int8_word64 . castDoubleToWord64

-- | The wire format is a sequence of base-10000 digit groups, most
-- significant first, along with the number of groups, the position of the
-- decimal point (in groups), the sign and the scale.
--
-- Note that the header fields are validated to fit their 16 bit wire
-- representation, which the upstream encoder doesn't do: a value with a
-- large enough exponent would otherwise wrap around and be stored as an
-- entirely different number.
numeric :: S.Scientific -> Encoding
numeric value
  | groupCount > maxWord16 = tooLarge "number of digit groups" groupCount
  | pointIndex > maxInt16 || pointIndex < minInt16 = tooLarge "weight" pointIndex
  | scale > maxWord16 = tooLarge "scale" scale
  | otherwise =
      mconcat
        [ int2_word16 $ fromIntegral groupCount
        , int2_word16 $ fromIntegral pointIndex
        , int2_word16 signCode
        , int2_word16 $ fromIntegral scale
        , foldMap int2_word16 groups
        ]
  where
    coefficient = S.coefficient value
    exponent_ = S.base10Exponent value

    -- Digit groups are aligned to multiples of four decimal digits, so the
    -- coefficient is scaled up until the exponent is such a multiple.
    (alignedCoefficient, alignedExponent) = case exponent_ `mod` 4 of
      0 -> (coefficient, exponent_)
      n -> (coefficient * 10 ^ n, exponent_ - n)

    groups = digitGroups alignedCoefficient
    groupCount = length groups
    pointIndex = groupCount + (alignedExponent `div` 4) - 1
    scale = max 0 $ negate alignedExponent
    signCode = if coefficient < 0 then 0x4000 else 0x0000

    -- Decompose the absolute value into base-10000 digits, most significant
    -- first.
    digitGroups :: Integer -> [Word16]
    digitGroups =
      reverse . L.unfoldr (\n -> if n == 0 then Nothing else Just (swap $ n `divMod` 10000)) . abs
      where
        swap (d, m) = (fromIntegral m, d)

    maxWord16 = fromIntegral (maxBound :: Word16)
    maxInt16 = fromIntegral (maxBound :: Int16)
    minInt16 = fromIntegral (minBound :: Int16)

    tooLarge :: String -> Int -> a
    tooLarge what n =
      throw . HPQTypesError $
        "numeric: "
          ++ what
          ++ " ("
          ++ show n
          ++ ") of "
          ++ show value
          ++ " is outside the range representable by the wire format"

----------------------------------------
-- Character types

-- Note: the bytes are passed through verbatim. The upstream encoders drop
-- NUL characters instead, which silently corrupts the value; the server
-- rejects them by itself with a clear error.

text_strict :: T.Text -> Encoding
text_strict = bytea_strict . T.encodeUtf8

text_lazy :: TL.Text -> Encoding
text_lazy = bytea_lazy . TL.encodeUtf8

----------------------------------------
-- Byte arrays

bytea_strict :: BS.ByteString -> Encoding
bytea_strict = Encoding . B.bytes

bytea_lazy :: BSL.ByteString -> Encoding
bytea_lazy = Encoding . B.lazyBytes

----------------------------------------
-- Date and time

-- Note: the components of the time values are taken apart with the accessors
-- of the "Data.Time" and "Data.Fixed" types rather than by coercing them to
-- their representations.

-- | Days since the PostgreSQL epoch (2000-01-01), which the server uses
-- internally instead of the Modified Julian Date.
dayToPostgresJulian :: Day -> Integer
dayToPostgresJulian = subtract 51544 . toModifiedJulianDay

date :: Day -> Encoding
date = int4_int32 . fromIntegral . dayToPostgresJulian

time_int :: TimeOfDay -> Encoding
time_int (TimeOfDay hours minutes seconds) =
  int8_int64 $
    picosecondsToMicros seconds
      + 1000000 * 60 * (fromIntegral minutes + 60 * fromIntegral hours)
  where
    picosecondsToMicros (MkFixed picos) = fromIntegral $ picos `div` 1000000

timestamp_int :: LocalTime -> Encoding
timestamp_int (LocalTime day time) =
  int8_int64 $
    microsPerDay * fromIntegral (dayToPostgresJulian day)
      + fromIntegral (diffTimeToPicoseconds (timeOfDayToTime time) `div` 1000000)

timestamptz_int :: UTCTime -> Encoding
timestamptz_int (UTCTime day time) =
  int8_int64 $
    microsPerDay * fromIntegral (dayToPostgresJulian day)
      + fromIntegral (diffTimeToPicoseconds time `div` 1000000)

microsPerDay :: Int64
microsPerDay = 1000000 * 60 * 60 * 24

----------------------------------------
-- Miscellaneous

bool :: Bool -> Encoding
bool b = Encoding . B.word8 $ if b then 1 else 0

uuid :: U.UUID -> Encoding
uuid value = case U.toWords value of
  (w1, w2, w3, w4) -> int4_word32 w1 <> int4_word32 w2 <> int4_word32 w3 <> int4_word32 w4

-- | An address along with the length of its netmask. This is the wire format
-- of both @inet@ and @cidr@; the flag distinguishing them is set from the
-- OID of the parameter, so it's always encoded as @inet@ here (the server
-- accepts that for a @cidr@ parameter as long as no bits are set to the
-- right of the netmask, which is what makes a value a @cidr@ in the first
-- place).
inet :: (IP.IP, Int) -> Encoding
inet (address, maskLen) = case address of
  IP.IPv4 addr ->
    header inetAddressFamily 4 <> int4_word32 (IP.fromIPv4w addr)
  IP.IPv6 addr -> case IP.fromIPv6w addr of
    (w1, w2, w3, w4) ->
      header inet6AddressFamily 16
        <> int4_word32 w1
        <> int4_word32 w2
        <> int4_word32 w3
        <> int4_word32 w4
  where
    -- Address family, netmask length, the is-cidr flag and the size of the
    -- address that follows.
    header family size =
      Encoding $
        B.word8 family
          <> B.word8 (fromIntegral maskLen)
          <> B.word8 0
          <> B.word8 size

    inetAddressFamily = 2 -- AF_INET
    inet6AddressFamily = 3 -- AF_INET6

----------------------------------------
-- JSON

json_bytes :: BS.ByteString -> Encoding
json_bytes = bytea_strict

json_bytes_lazy :: BSL.ByteString -> Encoding
json_bytes_lazy = bytea_lazy

json_ast :: A.Value -> Encoding
json_ast = bytea_lazy . A.encode

-- The wire format of jsonb is its JSON text prefixed with a version number.

jsonbVersion :: Encoding
jsonbVersion = Encoding $ B.word8 1

jsonb_bytes :: BS.ByteString -> Encoding
jsonb_bytes value = jsonbVersion <> bytea_strict value

jsonb_bytes_lazy :: BSL.ByteString -> Encoding
jsonb_bytes_lazy value = jsonbVersion <> bytea_lazy value

jsonb_ast :: A.Value -> Encoding
jsonb_ast value = jsonbVersion <> json_ast value

----------------------------------------
-- Ranges

-- | The wire format is a flags byte followed by the length-prefixed bounds
-- that are neither infinite nor absent.
range :: (a -> Encoding) -> Range a -> Encoding
range encodeBound = \case
  Empty -> flags 0x01
  Range Inf Inf -> flags 0x18
  Range (Excl l) (Excl u) -> flags 0x00 <> bound l <> bound u
  Range (Incl l) (Excl u) -> flags 0x02 <> bound l <> bound u
  Range (Excl l) (Incl u) -> flags 0x04 <> bound l <> bound u
  Range (Incl l) (Incl u) -> flags 0x06 <> bound l <> bound u
  Range (Excl l) Inf -> flags 0x10 <> bound l
  Range (Incl l) Inf -> flags 0x12 <> bound l
  Range Inf (Excl u) -> flags 0x08 <> bound u
  Range Inf (Incl u) -> flags 0x0c <> bound u
  where
    flags = Encoding . B.word8
    bound = sized . encodeBound

int4range :: Range Int32 -> Encoding
int4range = range int4_int32

int8range :: Range Int64 -> Encoding
int8range = range int8_int64

numrange :: Range S.Scientific -> Encoding
numrange = range numeric

daterange :: Range Day -> Encoding
daterange = range date

tsrange_int :: Range LocalTime -> Encoding
tsrange_int = range timestamp_int

tstzrange_int :: Range UTCTime -> Encoding
tstzrange_int = range timestamptz_int
