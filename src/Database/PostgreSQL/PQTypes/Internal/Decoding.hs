-- | Decoders of the PostgreSQL binary wire format.
--
-- The decoders derive from the @postgresql-binary@ package by Nikita Volkov
-- (MIT license, see @LICENSE@). They cover only the types that this library
-- supports. Where the upstream behavior didn't suit us, they diverge:
--
-- * If the length prefix of a value inside a container is below @-1@ (the
--   value that stands for NULL), the decoder rejects it. Upstream passes it
--   on to @unsafeTake@ and @unsafeDrop@, which yields a 'BS.ByteString' of
--   negative length that points before the start of its buffer. This
--   concerns every container decoder. Here it's the bounds of the ranges.
--
-- * 'float4' and 'float8' interpret the bit pattern with the casts from
--   "GHC.Float". Upstream coerces the value. The coercion is undefined
--   behavior and reads the wrong bytes on a big-endian architecture.
--
-- * 'jsonb_bytes' rejects an unknown version byte. Upstream discards the
--   byte unread.
--
-- * The date and time decoders reject the values that stand for @infinity@
--   and @-infinity@. The "Data.Time" types cannot represent them. Upstream
--   decodes them as ordinary values and silently yields a date far outside
--   the range that the server accepts. 'numeric' likewise says which special
--   value it got instead of reporting an unexpected sign code.
--
-- * The time decoders assemble the values with the constructors of the
--   "Data.Time" and "Data.Fixed" types, not with a coercion of their
--   representations.
--
-- * t'Value' is a newtype, not an alias of the underlying parser, so that the
--   parser library doesn't leak into the public API of the library. You
--   build custom decoders with 'fn' and 'refine'.
--
-- * 'inet' decodes the address and the length of its netmask. The caller
--   maps them onto a Haskell type. Upstream decodes into 'IP.IPRange', which
--   silently discards the host bits of the address.
module Database.PostgreSQL.PQTypes.Internal.Decoding
  ( -- * Decoding
    Value
  , valueParser

    -- ** Custom decoders
  , fn
  , refine

    -- * Numbers
  , int
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
  , json_aeson
  , jsonb_bytes
  , jsonb_aeson

    -- * Ranges
  , int4range
  , int8range
  , numrange
  , daterange
  , tsrange_int
  , tstzrange_int
  ) where

import BinaryParser qualified as BP
import Data.Aeson qualified as A
import Data.Bits
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Fixed (Fixed (..), Pico)
import Data.IP
import Data.Int
import Data.Scientific qualified as S
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Encoding.Error qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Time
import Data.Typeable
import Data.UUID.Types qualified as U
import Data.Vector qualified as V
import Data.Word
import GHC.Float (castWord32ToFloat, castWord64ToDouble)

import Database.PostgreSQL.PQTypes.Range

-- | Decoder of a single value from its payload in the PostgreSQL binary wire
-- format.
--
-- Build custom decoders with 'fn' and 'refine' on top of the ones defined
-- here. Compose them with the 'Functor', 'Applicative' and 'Monad'
-- interfaces.
newtype Value a = Value (BP.BinaryParser a)
  deriving newtype (Applicative, Functor, Monad)

-- | Run a decoder against the payload of a value.
valueParser :: Value a -> BS.ByteString -> Either T.Text a
valueParser (Value parser) = BP.run parser

-- | Define a decoder from a function that parses the whole payload.
fn :: (BS.ByteString -> Either T.Text a) -> Value a
fn f = Value $ BP.remainders >>= either BP.failure pure . f

-- | Narrow an existing decoder with a function that can reject its result.
refine :: (a -> Either T.Text b) -> Value a -> Value b
refine f value = value >>= Value . either BP.failure pure . f

----------------------------------------
-- Helpers

failure :: T.Text -> Value a
failure = Value . BP.failure

-- | Read an integer of the given size in bytes, big endian.
intOfSize :: (Bits a, Num a) => Int -> Value a
intOfSize size = Value $ pack <$> BP.bytesOfSize size
  where
    pack = BS.foldl' (\n byte -> shiftL n 8 .|. fromIntegral byte) 0

-- | Read a length-prefixed payload with the given decoder. A length of @-1@
-- represents NULL. The elements of the containers use it.
onContent :: Value a -> Value (Maybe a)
onContent (Value decoder) = do
  size <- intOfSize @Int32 4
  case compare size (-1) of
    LT -> failure $ "Negative length of a value: " <> T.pack (show size)
    EQ -> pure Nothing
    GT -> Value . fmap Just . BP.sized (fromIntegral size) $ decoder

nonNull :: Maybe a -> Value a
nonNull = maybe (failure "Unexpected NULL") pure

----------------------------------------
-- Numbers

-- | Any integral type, decoded from the whole payload.
int :: (Bits a, Num a) => Value a
int = Value $ pack <$> BP.remainders
  where
    pack = BS.foldl' (\n byte -> shiftL n 8 .|. fromIntegral byte) 0

float4 :: Value Float
float4 = castWord32ToFloat <$> int

float8 :: Value Double
float8 = castWord64ToDouble <$> int

-- | The wire format is a sequence of base-10000 digit groups, most
-- significant first, along with the number of groups, the position of the
-- decimal point (in groups), the sign and the scale. The decoder doesn't
-- need the scale to reconstruct the value.
numeric :: Value S.Scientific
numeric = do
  groupCount <- intOfSize @Int 2
  pointIndex <- intOfSize @Int16 2
  signCode <- intOfSize @Word16 2
  -- Skip the scale.
  Value $ BP.unitOfSize 2
  groups <- V.replicateM groupCount $ intOfSize @Word16 2
  sign <- case signCode of
    0x0000 -> pure id
    0x4000 -> pure negate
    0xc000 -> unrepresentable "NaN"
    0xd000 -> unrepresentable "Infinity"
    0xf000 -> unrepresentable "-Infinity"
    _ -> failure $ "Unexpected numeric sign code: " <> T.pack (show signCode)
  let coefficient = V.foldl' (\acc group -> acc * 10000 + fromIntegral group) 0 groups
      exponent_ = (fromIntegral pointIndex + 1 - V.length groups) * 4
  pure $ S.scientific (sign coefficient) exponent_
  where
    -- The special values of numeric, none of which Scientific can hold.
    unrepresentable :: T.Text -> Value a
    unrepresentable value =
      failure $ "numeric '" <> value <> "' cannot be represented by Scientific"

----------------------------------------
-- Character types

text_strict :: Value T.Text
text_strict = fn $ either (Left . decodeError) Right . T.decodeUtf8'

text_lazy :: Value TL.Text
text_lazy = fn $ either (Left . decodeError) Right . TL.decodeUtf8' . BSL.fromStrict

decodeError :: T.UnicodeException -> T.Text
decodeError = \case
  T.DecodeError err _ -> "Failed to decode the value in UTF-8: " <> T.pack err
  err -> T.pack $ show err

----------------------------------------
-- Byte arrays

bytea_strict :: Value BS.ByteString
bytea_strict = Value BP.remainders

bytea_lazy :: Value BSL.ByteString
bytea_lazy = BSL.fromStrict <$> bytea_strict

----------------------------------------
-- Date and time

-- | The inverse of the conversion that the encoder does: the server counts
-- days from 2000-01-01.
postgresJulianToDay :: Integral a => a -> Day
postgresJulianToDay = ModifiedJulianDay . (+ 51544) . fromIntegral

-- | Reject the extremes of the wire representation. They stand for
-- @infinity@ and @-infinity@, which the "Data.Time" types cannot represent.
-- If they were decoded, the result would be a date far outside the range
-- that the server accepts.
finite :: (Bounded a, Eq a) => T.Text -> T.Text -> a -> Value a
finite pgType haskellType n
  | n == maxBound = unrepresentable "infinity"
  | n == minBound = unrepresentable "-infinity"
  | otherwise = pure n
  where
    unrepresentable value =
      failure $ pgType <> " '" <> value <> "' cannot be represented by " <> haskellType

date :: Value Day
date = postgresJulianToDay <$> (int @Int32 >>= finite "date" "Day")

time_int :: Value TimeOfDay
time_int = microsToTimeOfDay <$> int

timestamp_int :: Value LocalTime
timestamp_int = do
  (day, micros) <- splitDay <$> (int >>= finite "timestamp" "LocalTime")
  pure $ LocalTime day (microsToTimeOfDay micros)

timestamptz_int :: Value UTCTime
timestamptz_int = do
  (day, micros) <- splitDay <$> (int >>= finite "timestamptz" "UTCTime")
  pure . UTCTime day . picosecondsToDiffTime $ fromIntegral micros * 1000000

splitDay :: Int64 -> (Day, Int64)
splitDay micros = case micros `divMod` (1000000 * 60 * 60 * 24) of
  (days, rest) -> (postgresJulianToDay days, rest)

microsToTimeOfDay :: Int64 -> TimeOfDay
microsToTimeOfDay micros = TimeOfDay (fromIntegral hours) (fromIntegral minutes) seconds
  where
    (hours, afterHours) = micros `divMod` (1000000 * 60 * 60)
    (minutes, afterMinutes) = afterHours `divMod` (1000000 * 60)
    seconds = MkFixed (fromIntegral afterMinutes * 1000000) :: Pico

----------------------------------------
-- Miscellaneous

bool :: Value Bool
bool = (== 1) <$> intOfSize @Word8 1

uuid :: Value U.UUID
uuid = U.fromWords <$> intOfSize 4 <*> intOfSize 4 <*> intOfSize 4 <*> intOfSize 4

-- | An address along with the length of its netmask. This is the wire format
-- of both @inet@ and @cidr@. The two differ only in their OIDs, which the
-- caller tells apart, and in a flag that is of no use to the decoder. A
-- @cidr@ value has no bits set to the right of the netmask.
inet :: Value (IP, Int)
inet = do
  family <- intOfSize @Word8 1
  maskLen <- intOfSize @Int 1
  -- Skip the is-cidr flag and the size of the address.
  _isCidr <- intOfSize @Word8 1
  _size <- intOfSize @Word8 1
  address <- case family of
    -- AF_INET
    2 -> IPv4 . toIPv4w <$> intOfSize 4
    -- AF_INET6
    3 ->
      IPv6 . toIPv6w
        <$> ((,,,) <$> intOfSize 4 <*> intOfSize 4 <*> intOfSize 4 <*> intOfSize 4)
    _ -> failure $ "Unknown address family: " <> T.pack (show family)
  pure (address, maskLen)

----------------------------------------
-- JSON

-- | Decode a @json@ value with the given parser of its UTF-8 text.
json_bytes :: (BS.ByteString -> Either T.Text a) -> Value a
json_bytes = fn

-- | Decode a @json@ value with the 'A.FromJSON' instance of its type.
json_aeson :: (A.FromJSON a, Typeable a) => Value a
json_aeson = json_bytes decodeJson

-- | Decode a @jsonb@ value with the given parser of its UTF-8 text. On the
-- wire, @jsonb@ is plain JSON text with a version number prefix.
jsonb_bytes :: (BS.ByteString -> Either T.Text a) -> Value a
jsonb_bytes parse = do
  version <- intOfSize @Word8 1
  if version == 1
    then Value BP.remainders >>= Value . either BP.failure pure . parse
    else failure $ "Unexpected jsonb version: " <> T.pack (show version)

-- | Decode a @jsonb@ value with the 'A.FromJSON' instance of its type.
jsonb_aeson :: (A.FromJSON a, Typeable a) => Value a
jsonb_aeson = jsonb_bytes decodeJson

-- | Decode JSON text with the 'A.FromJSON' instance of the target type. An
-- error names the type, because the message of the instance doesn't
-- always do so.
decodeJson :: forall a. (A.FromJSON a, Typeable a) => BS.ByteString -> Either T.Text a
decodeJson = either (Left . withType . T.pack) Right . A.eitherDecodeStrict'
  where
    withType :: T.Text -> T.Text
    withType err =
      "cannot decode JSON as " <> T.pack (show $ typeRep (Proxy @a)) <> ": " <> err

----------------------------------------
-- Ranges

-- | The wire format is a flags byte followed by the length-prefixed bounds
-- that are neither infinite nor absent.
range :: Value a -> Value (Range a)
range decodeBound = do
  flags <- intOfSize @Word8 1
  let bound infiniteBit inclusiveBit
        | testBit flags infiniteBit = pure Inf
        | otherwise =
            (if testBit flags inclusiveBit then Incl else Excl)
              <$> (nonNull =<< onContent decodeBound)
  if testBit flags 0
    then pure Empty
    else Range <$> bound 3 1 <*> bound 4 2

int4range :: Value (Range Int32)
int4range = range int

int8range :: Value (Range Int64)
int8range = range int

numrange :: Value (Range S.Scientific)
numrange = range numeric

daterange :: Value (Range Day)
daterange = range date

tsrange_int :: Value (Range LocalTime)
tsrange_int = range timestamp_int

tstzrange_int :: Value (Range UTCTime)
tstzrange_int = range timestamptz_int
