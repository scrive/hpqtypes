module Database.PostgreSQL.PQTypes.FromSQL
  ( FromSQL (..)

    -- * Row decoder
  , RowDecoder

    -- ** Combinators
  , decodeArray
  , decodeComposite
  , decodeEnum
  , decodeNullable
  , decodeScalar

    -- ** Generic decoding
  , genericDecoder
  , GFromSQL
  ) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Char
import Data.IP (IPRange)
import Data.Int
import Data.Scientific
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time
import Data.UUID.Types
import Data.Vector qualified as V
import Data.Word
import GHC.Generics
import GHC.TypeLits
import PostgreSQL.Binary.Decoding qualified as D
import PostgreSQL.Binary.Range (Range)
import TextShow

import Database.PostgreSQL.PQTypes.Internal.RowDecoder

-- | Class which represents \"from SQL type to Haskell type\" transformation.
-- Decoding of compound values is a matter of monadically composing decoders
-- of their fields:
--
-- > -- select 1::int, 'hi'::text, true::bool
-- > data T = T Int32 T.Text Bool
-- >
-- > decodeT :: RowDecoder T
-- > decodeT = T <$> fromSQL <*> fromSQL <*> fromSQL
--
-- See 'decodeNullable', 'decodeComposite' and 'decodeArray' for decoding
-- NULLable values, composite (anonymous row or user-defined) values and
-- arrays. Note that thanks to the 'Maybe' and list instances, once a type
-- has a 'FromSQL' instance, 'fromSQL' can also fetch its NULLable values and
-- arrays (of any dimension) directly, e.g. as @Maybe T@ or @[[T]]@.
--
-- Every 'fromSQL' consumes exactly one field of the result - even the
-- compound instances, e.g. the list instance decodes many values from a
-- single array field. Custom instances must preserve this invariant, since
-- the generic combinators rely on it (e.g. 'decodeNullable' checks the
-- NULL-ness of a single field before running the inner decoder). A decoder
-- of multiple consecutive fields is not a 'fromSQL', but a standalone
-- 'RowDecoder' composed of the decoders of the fields, like @decodeT@
-- above.
--
-- Time related instances use decoders for servers that have the
-- @integer_datetimes@ parameter set to @on@, which is the case for all
-- supported versions of PostgreSQL (the ability to compile the server with
-- floating point datetimes was removed in PostgreSQL 10).
class FromSQL a where
  -- | Decode a value from the next field of the query result.
  fromSQL :: RowDecoder a

  -- | Decoder used by the @'FromSQL' [a]@ instance. The default decodes a
  -- PostgreSQL array of @a@; the only reason for this method to exist is to
  -- allow 'String' to keep decoding text (see the 'Char' instance). Use
  -- 'fromSQL' at the list type instead of calling this method directly.
  fromSQLList :: RowDecoder [a]
  fromSQLList = V.toList <$> decodeArray fromSQL
  -- Inlined so that instances in other modules (in particular ones defined
  -- with 'genericDecoder') specialize the element decoder instead of
  -- calling the generically compiled default.
  {-# INLINE fromSQLList #-}

-- GENERIC DECODING

-- | Generic decoder of consecutive fields of a row into a product type,
-- e.g. for fetching rows as tuples without composing the decoders of their
-- fields by hand:
--
-- > (n, t) <- fetchOne $ genericDecoder @(Int32, T.Text)
--
-- Combined with 'decodeComposite', it gives 'FromSQL' instances for product
-- types corresponding to composite types:
--
-- > data Simple = Simple (Maybe Int32) (Maybe Day)
-- >   deriving stock (Generic, Show)
-- >
-- > instance FromSQL Simple where
-- >   fromSQL = decodeComposite genericDecoder
genericDecoder :: (Generic a, GFromSQL (Rep a)) => RowDecoder a
genericDecoder = to <$> gFromSQL

-- | Decoder of the generic representation of a product type.
class GFromSQL f where
  gFromSQL :: RowDecoder (f p)

instance GFromSQL f => GFromSQL (M1 i c f) where
  gFromSQL = M1 <$> gFromSQL

instance (GFromSQL f, GFromSQL g) => GFromSQL (f :*: g) where
  gFromSQL = (:*:) <$> gFromSQL <*> gFromSQL

instance FromSQL a => GFromSQL (K1 i a) where
  gFromSQL = K1 <$> fromSQL

instance GFromSQL U1 where
  gFromSQL = pure U1

instance
  TypeError (Text "Cannot derive FromSQL for a type with more than one constructor")
  => GFromSQL (f :+: g)
  where
  gFromSQL = error "unreachable"

-- NULLables

-- | 'Nothing' represents NULL. Note that the type of a NULL field is not
-- checked, see 'decodeNullable'.
instance FromSQL a => FromSQL (Maybe a) where
  fromSQL = decodeNullable fromSQL

-- ARRAYS

instance FromSQL a => FromSQL [a] where
  fromSQL = fromSQLList

instance FromSQL a => FromSQL (V.Vector a) where
  fromSQL = decodeArray fromSQL

-- NUMERICS

instance FromSQL Int16 where
  fromSQL = decodeScalar D.int

instance FromSQL Int32 where
  fromSQL = decodeScalar D.int

instance FromSQL Int64 where
  fromSQL = decodeScalar D.int

instance FromSQL Float where
  fromSQL = decodeScalar D.float4

instance FromSQL Double where
  fromSQL = decodeScalar D.float8

instance FromSQL Word16 where
  fromSQL = decodeScalar D.int

instance FromSQL Word32 where
  fromSQL = decodeScalar D.int

instance FromSQL Word64 where
  fromSQL = decodeScalar D.int

instance FromSQL Integer where
  fromSQL = decodeScalar . (`D.refine` D.numeric) $ \n ->
    case floatingOrInteger @Double n of
      Left v -> Left $ "expected an integer, got " <> showt v
      Right i -> Right i

instance FromSQL Scientific where
  fromSQL = decodeScalar D.numeric

-- CHAR

-- The "char" type stores a single byte, hence characters above '\255' are not
-- representable and the decoding of the rest is the identity, not UTF-8.

singleByte :: D.Value Word8
singleByte = D.fn $ \value ->
  case BS.length value of
    1 -> Right $ BS.head value
    n -> Left $ "expected 1 byte, got " <> showt n

instance FromSQL Char where
  fromSQL = decodeScalar $ chr . fromIntegral <$> singleByte

  -- Decode 'String' from text instead of an array of "char".
  fromSQLList = decodeScalar $ T.unpack <$> D.text_strict

instance FromSQL Word8 where
  fromSQL = decodeScalar singleByte

-- VARIABLE-LENGTH CHARACTER TYPES

instance FromSQL T.Text where
  fromSQL = decodeScalar D.text_strict

instance FromSQL TL.Text where
  fromSQL = decodeScalar D.text_lazy

instance FromSQL UUID where
  fromSQL = decodeScalar D.uuid

-- BYTEA

instance FromSQL BS.ByteString where
  fromSQL = decodeScalar D.bytea_strict

instance FromSQL BSL.ByteString where
  fromSQL = decodeScalar D.bytea_lazy

-- DATE

instance FromSQL Day where
  fromSQL = decodeScalar D.date

-- TIME

instance FromSQL TimeOfDay where
  fromSQL = decodeScalar D.time_int

-- TIMESTAMP

instance FromSQL LocalTime where
  fromSQL = decodeScalar D.timestamp_int

-- TIMESTAMPTZ

-- Note: there is no 'FromSQL' instance for 'ZonedTime', as the binary
-- representation of timestamptz doesn't carry the time zone offset.

instance FromSQL UTCTime where
  fromSQL = decodeScalar D.timestamptz_int

-- BOOL

instance FromSQL Bool where
  fromSQL = decodeScalar D.bool

-- INET

-- | Note that there is no instance for the @cidr@ type, which shares the
-- wire format with @inet@, but has a different OID.
instance FromSQL IPRange where
  fromSQL = decodeScalar D.inet

-- RANGES

instance FromSQL (Range Int32) where
  fromSQL = decodeScalar D.int4range

instance FromSQL (Range Int64) where
  fromSQL = decodeScalar D.int8range

instance FromSQL (Range Scientific) where
  fromSQL = decodeScalar D.numrange

instance FromSQL (Range Day) where
  fromSQL = decodeScalar D.daterange

instance FromSQL (Range LocalTime) where
  fromSQL = decodeScalar D.tsrange_int

instance FromSQL (Range UTCTime) where
  fromSQL = decodeScalar D.tstzrange_int
