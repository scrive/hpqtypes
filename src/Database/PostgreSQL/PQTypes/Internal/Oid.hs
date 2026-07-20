-- | The 'Oid' type along with OIDs of built-in PostgreSQL types.
module Database.PostgreSQL.PQTypes.Internal.Oid
  ( Oid (..)
  , unOid

    -- * OIDs of built-in types
  , boolOid
  , boolArrayOid
  , byteaOid
  , byteaArrayOid
  , charOid
  , charArrayOid
  , nameOid
  , nameArrayOid
  , int8Oid
  , int8ArrayOid
  , int2Oid
  , int2ArrayOid
  , int4Oid
  , int4ArrayOid
  , textOid
  , textArrayOid
  , jsonOid
  , jsonArrayOid
  , xmlOid
  , xmlArrayOid
  , float4Oid
  , float4ArrayOid
  , float8Oid
  , float8ArrayOid
  , unknownOid
  , inetOid
  , inetArrayOid
  , bpcharOid
  , bpcharArrayOid
  , varcharOid
  , varcharArrayOid
  , dateOid
  , dateArrayOid
  , timeOid
  , timeArrayOid
  , timestampOid
  , timestampArrayOid
  , timestamptzOid
  , timestamptzArrayOid
  , intervalOid
  , intervalArrayOid
  , timetzOid
  , timetzArrayOid
  , numericOid
  , numericArrayOid
  , recordOid
  , recordArrayOid
  , uuidOid
  , uuidArrayOid
  , jsonbOid
  , jsonbArrayOid
  , int4rangeOid
  , int4rangeArrayOid
  , numrangeOid
  , numrangeArrayOid
  , tsrangeOid
  , tsrangeArrayOid
  , tstzrangeOid
  , tstzrangeArrayOid
  , daterangeOid
  , daterangeArrayOid
  , int8rangeOid
  , int8rangeArrayOid
  , firstNormalOid
  , unspecifiedOid
  , pgTypeName
  ) where

import Data.Word
import Foreign.Storable

-- | Internal object identifier used by PostgreSQL, in particular for
-- identification of types of query parameters and result columns.
newtype Oid = Oid Word32
  deriving newtype (Eq, Ord, Storable)

unOid :: Oid -> Word32
unOid (Oid oid) = oid

instance Show Oid where
  showsPrec p oid@(Oid n) =
    showParen (p > 10) $
      showString "Oid "
        . shows n
        . showString " ("
        . showString (pgTypeName oid)
        . showString ")"

-- OIDs of built-in types are stable; the values below come from
-- pg_type.dat in the PostgreSQL sources.

boolOid, boolArrayOid :: Oid
boolOid = Oid 16
boolArrayOid = Oid 1000

byteaOid, byteaArrayOid :: Oid
byteaOid = Oid 17
byteaArrayOid = Oid 1001

charOid, charArrayOid :: Oid
charOid = Oid 18
charArrayOid = Oid 1002

nameOid, nameArrayOid :: Oid
nameOid = Oid 19
nameArrayOid = Oid 1003

int8Oid, int8ArrayOid :: Oid
int8Oid = Oid 20
int8ArrayOid = Oid 1016

int2Oid, int2ArrayOid :: Oid
int2Oid = Oid 21
int2ArrayOid = Oid 1005

int4Oid, int4ArrayOid :: Oid
int4Oid = Oid 23
int4ArrayOid = Oid 1007

textOid, textArrayOid :: Oid
textOid = Oid 25
textArrayOid = Oid 1009

jsonOid, jsonArrayOid :: Oid
jsonOid = Oid 114
jsonArrayOid = Oid 199

xmlOid, xmlArrayOid :: Oid
xmlOid = Oid 142
xmlArrayOid = Oid 143

float4Oid, float4ArrayOid :: Oid
float4Oid = Oid 700
float4ArrayOid = Oid 1021

float8Oid, float8ArrayOid :: Oid
float8Oid = Oid 701
float8ArrayOid = Oid 1022

unknownOid :: Oid
unknownOid = Oid 705

inetOid, inetArrayOid :: Oid
inetOid = Oid 869
inetArrayOid = Oid 1041

bpcharOid, bpcharArrayOid :: Oid
bpcharOid = Oid 1042
bpcharArrayOid = Oid 1014

varcharOid, varcharArrayOid :: Oid
varcharOid = Oid 1043
varcharArrayOid = Oid 1015

dateOid, dateArrayOid :: Oid
dateOid = Oid 1082
dateArrayOid = Oid 1182

timeOid, timeArrayOid :: Oid
timeOid = Oid 1083
timeArrayOid = Oid 1183

timestampOid, timestampArrayOid :: Oid
timestampOid = Oid 1114
timestampArrayOid = Oid 1115

timestamptzOid, timestamptzArrayOid :: Oid
timestamptzOid = Oid 1184
timestamptzArrayOid = Oid 1185

intervalOid, intervalArrayOid :: Oid
intervalOid = Oid 1186
intervalArrayOid = Oid 1187

timetzOid, timetzArrayOid :: Oid
timetzOid = Oid 1266
timetzArrayOid = Oid 1270

numericOid, numericArrayOid :: Oid
numericOid = Oid 1700
numericArrayOid = Oid 1231

recordOid, recordArrayOid :: Oid
recordOid = Oid 2249
recordArrayOid = Oid 2287

uuidOid, uuidArrayOid :: Oid
uuidOid = Oid 2950
uuidArrayOid = Oid 2951

jsonbOid, jsonbArrayOid :: Oid
jsonbOid = Oid 3802
jsonbArrayOid = Oid 3807

int4rangeOid, int4rangeArrayOid :: Oid
int4rangeOid = Oid 3904
int4rangeArrayOid = Oid 3905

numrangeOid, numrangeArrayOid :: Oid
numrangeOid = Oid 3906
numrangeArrayOid = Oid 3907

tsrangeOid, tsrangeArrayOid :: Oid
tsrangeOid = Oid 3908
tsrangeArrayOid = Oid 3909

tstzrangeOid, tstzrangeArrayOid :: Oid
tstzrangeOid = Oid 3910
tstzrangeArrayOid = Oid 3911

daterangeOid, daterangeArrayOid :: Oid
daterangeOid = Oid 3912
daterangeArrayOid = Oid 3913

int8rangeOid, int8rangeArrayOid :: Oid
int8rangeOid = Oid 3926
int8rangeArrayOid = Oid 3927

-- | The lower bound of OIDs of user-defined objects
-- (FirstNormalObjectId in the PostgreSQL sources). All
-- built-in types have OIDs below it.
firstNormalOid :: Oid
firstNormalOid = Oid 16384

-- | Used as a type of a query parameter, it makes the server infer the type
-- of the parameter from the context in which it's used.
unspecifiedOid :: Oid
unspecifiedOid = Oid 0

-- | The name of the PostgreSQL type associated with a given 'Oid' of one of
-- the built-in types defined in this module.
pgTypeName :: Oid -> String
pgTypeName oid
  | oid == unspecifiedOid = "unspecified"
  | Just name <- lookup oid builtinTypeNames = name
  | oid >= firstNormalOid = "user-defined type"
  | otherwise = "unrecognized type"

builtinTypeNames :: [(Oid, String)]
builtinTypeNames =
  (unknownOid, "unknown")
    : concatMap
      (\(o, ao, name) -> [(o, name), (ao, name ++ "[]")])
      [ (boolOid, boolArrayOid, "bool")
      , (byteaOid, byteaArrayOid, "bytea")
      , (charOid, charArrayOid, "\"char\"")
      , (nameOid, nameArrayOid, "name")
      , (int8Oid, int8ArrayOid, "int8")
      , (int2Oid, int2ArrayOid, "int2")
      , (int4Oid, int4ArrayOid, "int4")
      , (textOid, textArrayOid, "text")
      , (jsonOid, jsonArrayOid, "json")
      , (xmlOid, xmlArrayOid, "xml")
      , (float4Oid, float4ArrayOid, "float4")
      , (float8Oid, float8ArrayOid, "float8")
      , (inetOid, inetArrayOid, "inet")
      , (bpcharOid, bpcharArrayOid, "bpchar")
      , (varcharOid, varcharArrayOid, "varchar")
      , (dateOid, dateArrayOid, "date")
      , (timeOid, timeArrayOid, "time")
      , (timestampOid, timestampArrayOid, "timestamp")
      , (timestamptzOid, timestamptzArrayOid, "timestamptz")
      , (intervalOid, intervalArrayOid, "interval")
      , (timetzOid, timetzArrayOid, "timetz")
      , (numericOid, numericArrayOid, "numeric")
      , (recordOid, recordArrayOid, "record")
      , (uuidOid, uuidArrayOid, "uuid")
      , (jsonbOid, jsonbArrayOid, "jsonb")
      , (int4rangeOid, int4rangeArrayOid, "int4range")
      , (numrangeOid, numrangeArrayOid, "numrange")
      , (tsrangeOid, tsrangeArrayOid, "tsrange")
      , (tstzrangeOid, tstzrangeArrayOid, "tstzrange")
      , (daterangeOid, daterangeArrayOid, "daterange")
      , (int8rangeOid, int8rangeArrayOid, "int8range")
      ]
