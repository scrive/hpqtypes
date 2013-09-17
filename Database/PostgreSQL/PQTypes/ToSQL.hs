{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses
  , RecordWildCards, ScopedTypeVariables, UndecidableInstances #-}
module Database.PostgreSQL.PQTypes.ToSQL {-(ToSQL(..))-} where

import Control.Monad
import Data.ByteString.Unsafe
import Data.Int
import Data.Text (Text)
import Data.Text.Encoding
import Data.Time
import Data.Word
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Unboxed as V

import Database.PostgreSQL.PQTypes.Internal.C.Interface
import Database.PostgreSQL.PQTypes.Internal.C.Put
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.Internal.Utils
import Database.PostgreSQL.PQTypes.Types

class Storable base => ToSQL dest base | dest -> base where
  pqFormatPut :: dest -> BS.ByteString
  toSQL       :: Ptr PGconn -> dest -> (Maybe base -> IO r) -> IO r

-- NULLables

instance ToSQL dest base => ToSQL (Maybe dest) base where
  pqFormatPut _ = pqFormatPut (undefined::dest)
  toSQL conn mdest conv = case mdest of
    Nothing   -> conv Nothing
    Just dest -> toSQL conn dest conv

-- NUMERICS

instance ToSQL Int16 CShort where
  pqFormatPut _ = BS.pack "%int2"
  toSQL _ n conv = conv . Just . fromIntegral $ n

instance ToSQL Int32 CInt where
  pqFormatPut _ = BS.pack "%int4"
  toSQL _ n conv = conv . Just . fromIntegral $ n

instance ToSQL Int64 CLLong where
  pqFormatPut _ = BS.pack "%int8"
  toSQL _ n conv = conv . Just . fromIntegral $ n

instance ToSQL Float CFloat where
  pqFormatPut _ = BS.pack "%float4"
  toSQL _ n conv = conv . Just . realToFrac $ n

instance ToSQL Double CDouble where
  pqFormatPut _ = BS.pack "%float8"
  toSQL _ n conv = conv . Just . realToFrac $ n

-- ARRAYS

instance (ToSQL dest base, PQPut base) => ToSQL (Array dest) (Ptr PGarray) where
  pqFormatPut _ = pqFormatPut (undefined::dest) `BS.append` BS.pack "[]"
  toSQL conn (Array arr) conv = alloca $ \ptr -> withPGparam conn $ \param -> do
    BS.useAsCString (pqFormatPut (undefined::dest)) $ \fmt -> forM_ arr $ \item ->
      verifyPQTRes "toSQL (Array)" =<< toSQL conn item (c_PQPutfMaybe param fmt)
    poke ptr PGarray {
      pgArrayNDims = 0
    , pgArrayLBound = V.empty
    , pgArrayDims = V.empty
    , pgArrayParam = param
    , pgArrayRes = nullPtr
    }
    conv . Just $ ptr

-- CHAR

instance ToSQL Char CChar where
  pqFormatPut _ = BS.pack "%char"
  toSQL _ c conv
    | c > '\255' = E.throwIO . InternalError $ "toSQL (Char): character " ++ show c ++ " cannot be losslessly converted to CChar"
    | otherwise = conv . Just . castCharToCChar $ c

instance ToSQL Word8 CChar where
  pqFormatPut _ = BS.pack "%char"
  toSQL _ c conv = conv . Just . fromIntegral $ c

-- VARIABLE-LENGTH CHARACTER TYPES

instance ToSQL BS.ByteString CString where
  pqFormatPut _ = BS.pack "%text"
  toSQL _ bs conv = BS.useAsCString bs $ \cs -> conv . Just $ cs

instance ToSQL Text CString where
  pqFormatPut _ = BS.pack "%text"
  toSQL conn = toSQL conn . encodeUtf8

instance ToSQL String CString where
  pqFormatPut _ = BS.pack "%text"
  toSQL _ s conv = withCString s $ \cs -> conv . Just $ cs

-- BYTEA

instance ToSQL (Binary BS.ByteString) (Ptr PGbytea) where
  pqFormatPut _ = BS.pack "%bytea"
  toSQL _ (Binary bs) conv = alloca $ \ptr ->
    unsafeUseAsCStringLen bs $ \(cs, len) -> do
      poke ptr PGbytea {
        pgByteaLen = fromIntegral len
      , pgByteaData = cs
      }
      conv . Just $ ptr

-- DATE

instance ToSQL Day (Ptr PGdate) where
  pqFormatPut _ = BS.pack "%date"
  toSQL _ day conv = alloca $ \ptr -> do
    poke ptr $ dayToPGdate day
    conv . Just $ ptr

-- TIME

instance ToSQL TimeOfDay (Ptr PGtime) where
  pqFormatPut _ = BS.pack "%time"
  toSQL _ tod conv = alloca $ \ptr -> do
    poke ptr $ timeOfDayToPGtime tod
    conv . Just $ ptr

-- TIMESTAMP

instance ToSQL LocalTime (Ptr PGtimestamp) where
  pqFormatPut _ = BS.pack "%timestamp"
  toSQL _ LocalTime{..} conv = alloca $ \ptr -> do
    poke ptr $ PGtimestamp {
      pgTimestampEpoch = 0
    , pgTimestampDate = dayToPGdate localDay
    , pgTimestampTime = timeOfDayToPGtime localTimeOfDay
    }
    conv . Just $ ptr

-- TIMESTAMPTZ

instance ToSQL UTCTime (Ptr PGtimestamp) where
  pqFormatPut _ = BS.pack "%timestamptz"
  toSQL _ UTCTime{..} conv = alloca $ \ptr -> do
    poke ptr $ PGtimestamp {
      pgTimestampEpoch = 0
    , pgTimestampDate = dayToPGdate utctDay
    , pgTimestampTime = timeOfDayToPGtime $ timeToTimeOfDay utctDayTime
    }
    conv . Just $ ptr

instance ToSQL ZonedTime (Ptr PGtimestamp) where
  pqFormatPut _ = BS.pack "%timestamptz"
  toSQL _ ZonedTime{..} conv = alloca $ \ptr -> do
    poke ptr $ PGtimestamp {
      pgTimestampEpoch = 0
    , pgTimestampDate = dayToPGdate $ localDay zonedTimeToLocalTime
    , pgTimestampTime = (timeOfDayToPGtime $ localTimeOfDay zonedTimeToLocalTime) {
        pgTimeGMTOff = fromIntegral (timeZoneMinutes zonedTimeZone) * 60
      }
    }
    conv . Just $ ptr

----------------------------------------

timeOfDayToPGtime :: TimeOfDay -> PGtime
timeOfDayToPGtime TimeOfDay{..} = PGtime {
    pgTimeHour = fromIntegral todHour
  , pgTimeMin = fromIntegral todMin
  , pgTimeSec = sec
  , pgTimeUSec = usec
  , pgTimeWithTZ = 0
  , pgTimeIsDST = 0
  , pgTimeGMTOff = 0
  , pgTimeTZAbbr = BS.empty
  }
  where
    (sec, usec) = floor ((toRational todSec) * 1000000) `divMod` 1000000

dayToPGdate :: Day -> PGdate
dayToPGdate day = PGdate {
    pgDateIsBC = isBC
  , pgDateYear = fromIntegral $ adjustBC year
  , pgDateMon = fromIntegral $ mon - 1
  , pgDateMDay = fromIntegral mday
  , pgDateJDay = 0
  , pgDateYDay = 0
  , pgDateWDay = 0
  }
  where
    (year, mon, mday) = toGregorian day

    isBC = if year <= 0 then 1 else 0
    adjustBC = if isBC == 1 then succ . negate else id
