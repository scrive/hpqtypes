{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, Rank2Types
  , RecordWildCards, ScopedTypeVariables, TypeFamilies #-}
module Database.PostgreSQL.PQTypes.ToSQL (ToSQL(..)) where

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

import Database.PostgreSQL.PQTypes.Format
import Database.PostgreSQL.PQTypes.Internal.C.Put
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Error

type AllocParam = forall r. (Ptr PGparam -> IO r) -> IO r

class (PQFormat t, PQPut (PQDest t)) => ToSQL t where
  type PQDest t :: *
  toSQL :: t -> AllocParam -> (Maybe (PQDest t) -> IO r) -> IO r

-- NULLables

instance ToSQL t => ToSQL (Maybe t) where
  type PQDest (Maybe t) = PQDest t
  toSQL mt allocParam conv = case mt of
    Nothing -> conv Nothing
    Just t  -> toSQL t allocParam conv

-- NUMERICS

instance ToSQL Int16 where
  type PQDest Int16 = CShort
  toSQL n _ conv = conv . Just . fromIntegral $ n

instance ToSQL Int32 where
  type PQDest Int32 = CInt
  toSQL n _ conv = conv . Just . fromIntegral $ n

instance ToSQL Int64 where
  type PQDest Int64 = CLLong
  toSQL n _ conv = conv . Just . fromIntegral $ n

instance ToSQL Float where
  type PQDest Float = CFloat
  toSQL n _ conv = conv . Just . realToFrac $ n

instance ToSQL Double where
  type PQDest Double = CDouble
  toSQL n _ conv = conv . Just . realToFrac $ n

-- CHAR

instance ToSQL Char where
  type PQDest Char = CChar
  toSQL c _ conv
    | c > '\255' = E.throwIO . InternalError $ "toSQL (Char): character " ++ show c ++ " cannot be losslessly converted to CChar"
    | otherwise = conv . Just . castCharToCChar $ c

instance ToSQL Word8 where
  type PQDest Word8 = CChar
  toSQL c _ conv = conv . Just . fromIntegral $ c

-- VARIABLE-LENGTH CHARACTER TYPES

instance ToSQL BS.ByteString where
  type PQDest BS.ByteString = CString
  toSQL bs _ conv = BS.useAsCString bs $ \cs -> conv . Just $ cs

instance ToSQL Text where
  type PQDest Text = CString
  toSQL = toSQL . encodeUtf8

instance ToSQL String where
  type PQDest String = CString
  toSQL s _ conv = withCString s $ \cs -> conv . Just $ cs

-- DATE

instance ToSQL Day where
  type PQDest Day = Ptr PGdate
  toSQL day _ conv = alloca $ \ptr -> do
    poke ptr $ dayToPGdate day
    conv . Just $ ptr

-- TIME

instance ToSQL TimeOfDay where
  type PQDest TimeOfDay = Ptr PGtime
  toSQL tod _ conv = alloca $ \ptr -> do
    poke ptr $ timeOfDayToPGtime tod
    conv . Just $ ptr

-- TIMESTAMP

instance ToSQL LocalTime where
  type PQDest LocalTime = Ptr PGtimestamp
  toSQL LocalTime{..} _ conv = alloca $ \ptr -> do
    poke ptr $ PGtimestamp {
      pgTimestampEpoch = 0
    , pgTimestampDate = dayToPGdate localDay
    , pgTimestampTime = timeOfDayToPGtime localTimeOfDay
    }
    conv . Just $ ptr

-- TIMESTAMPTZ

instance ToSQL UTCTime where
  type PQDest UTCTime = Ptr PGtimestamp
  toSQL UTCTime{..} _ conv = alloca $ \ptr -> do
    poke ptr $ PGtimestamp {
      pgTimestampEpoch = 0
    , pgTimestampDate = dayToPGdate utctDay
    , pgTimestampTime = timeOfDayToPGtime $ timeToTimeOfDay utctDayTime
    }
    conv . Just $ ptr

instance ToSQL ZonedTime where
  type PQDest ZonedTime = Ptr PGtimestamp
  toSQL ZonedTime{..} _ conv = alloca $ \ptr -> do
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
    pgTimeHour   = fromIntegral todHour
  , pgTimeMin    = fromIntegral todMin
  , pgTimeSec    = sec
  , pgTimeUSec   = usec
  , pgTimeWithTZ = 0
  , pgTimeIsDST  = 0
  , pgTimeGMTOff = 0
  , pgTimeTZAbbr = BS.empty
  }
  where
    (sec, usec) = floor ((toRational todSec) * 1000000) `divMod` 1000000

dayToPGdate :: Day -> PGdate
dayToPGdate day = PGdate {
    pgDateIsBC  = isBC
  , pgDateYear  = fromIntegral $ adjustBC year
  , pgDateMon   = fromIntegral $ mon - 1
  , pgDateMDay  = fromIntegral mday
  , pgDateJDay  = 0
  , pgDateYDay  = 0
  , pgDateWDay  = 0
  }
  where
    (year, mon, mday) = toGregorian day

    isBC = if year <= 0 then 1 else 0
    adjustBC = if isBC == 1 then succ . negate else id
