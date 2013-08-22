{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, FunctionalDependencies, OverloadedStrings, RecordWildCards #-}
module DB.FromSQL where

import Data.ByteString.Char8 (ByteString)
import Data.Int
import Data.Ratio
import Data.Time
import Data.Time.Clock.POSIX
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr

import DB.Primitive.Interface
import DB.Primitive.Types

class Storable base => FromSQL base dest | dest -> base where
  pqTypesFormat :: dest -> ByteString
  fromSQL       :: Storable base => base -> dest

instance FromSQL CInt Int32 where
  pqTypesFormat _ = "%int4"
  fromSQL = fromIntegral

instance FromSQL PGtimestamp LocalTime where
  pqTypesFormat _ = "%timestamp"
  fromSQL PGtimestamp{..} = LocalTime day tod
    where
      day = fromGregorian year mon mday
      tod = TimeOfDay hour mins $ sec + fromRational (usec % 1000000)

      year = fromIntegral $ pgDateYear pgTimestampDate
      mon = fromIntegral $ pgDateMon pgTimestampDate + 1
      mday = fromIntegral $ pgDateMDay pgTimestampDate
      hour = fromIntegral $ pgTimeHour pgTimestampTime
      mins = fromIntegral $ pgTimeMin pgTimestampTime
      sec = fromIntegral $ pgTimeSec pgTimestampTime
      usec = fromIntegral $ pgTimeUSec pgTimestampTime

instance FromSQL PGtimestamp UTCTime where
  pqTypesFormat _ = "%timestamptz"
  fromSQL PGtimestamp{..} = posixSecondsToUTCTime $ fromIntegral pgTimestampEpoch
