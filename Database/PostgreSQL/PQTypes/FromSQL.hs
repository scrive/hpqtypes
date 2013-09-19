{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances
  , RecordWildCards, ScopedTypeVariables, TypeFamilies #-}
module Database.PostgreSQL.PQTypes.FromSQL (FromSQL(..)) where

import Control.Applicative
import Data.Int
import Data.Ratio
import Data.Text (Text)
import Data.Text.Encoding
import Data.Time
import Data.Word
import Foreign.C
import Foreign.Storable
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS

import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Error
import Database.PostgreSQL.PQTypes.Internal.Format
import Database.PostgreSQL.PQTypes.Internal.Utils
import Database.PostgreSQL.PQTypes.Types

class (PQFormat t, Storable (PQBase t)) => FromSQL t where
  type PQBase t :: *
  fromSQL :: Maybe (PQBase t) -> IO t

-- NULLables

instance FromSQL t => FromSQL (Maybe t) where
  type PQBase (Maybe t) = PQBase t
  fromSQL mbase = case mbase of
    Just _  -> Just <$> fromSQL mbase
    Nothing -> return Nothing

-- NUMERICS

instance FromSQL Int16 where
  type PQBase Int16 = CShort
  fromSQL Nothing = unexpectedNULL
  fromSQL (Just n) = return . fromIntegral $ n

instance FromSQL Int32 where
  type PQBase Int32 = CInt
  fromSQL Nothing = unexpectedNULL
  fromSQL (Just n) = return . fromIntegral $ n

instance FromSQL Int64 where
  type PQBase Int64 = CLLong
  fromSQL Nothing = unexpectedNULL
  fromSQL (Just n) = return . fromIntegral $ n

instance FromSQL Float where
  type PQBase Float = CFloat
  fromSQL Nothing = unexpectedNULL
  fromSQL (Just n) = return . realToFrac $ n

instance FromSQL Double where
  type PQBase Double = CDouble
  fromSQL Nothing = unexpectedNULL
  fromSQL (Just n) = return . realToFrac $ n

-- CHAR

instance FromSQL Char where
  type PQBase Char = CChar
  fromSQL Nothing = unexpectedNULL
  fromSQL (Just c) = return . castCCharToChar $ c

instance FromSQL Word8 where
  type PQBase Word8 = CChar
  fromSQL Nothing = unexpectedNULL
  fromSQL (Just c) = return . fromIntegral $ c

-- VARIABLE-LENGTH CHARACTER TYPES

instance FromSQL String where
  type PQBase String = CString
  fromSQL Nothing = unexpectedNULL
  fromSQL (Just cs) = peekCString cs

instance FromSQL BS.ByteString where
  type PQBase BS.ByteString = CString
  fromSQL Nothing = unexpectedNULL
  fromSQL (Just cs) = BS.packCString cs

instance FromSQL Text where
  type PQBase Text = CString
  fromSQL Nothing = unexpectedNULL
  fromSQL (Just cs) = either E.throwIO return . decodeUtf8' =<< BS.packCString cs

-- BYTEA

instance FromSQL (Binary BS.ByteString) where
  type PQBase (Binary BS.ByteString) = PGbytea
  fromSQL Nothing = unexpectedNULL
  fromSQL (Just PGbytea{..}) = Binary
    <$> BS.packCStringLen (pgByteaData, fromIntegral pgByteaLen)

-- DATE

instance FromSQL Day where
  type PQBase Day = PGdate
  fromSQL Nothing = unexpectedNULL
  fromSQL (Just date) = return . pgDateToDay $ date

-- TIME

instance FromSQL TimeOfDay where
  type PQBase TimeOfDay = PGtime
  fromSQL Nothing = unexpectedNULL
  fromSQL (Just time) = return . pgTimeToTimeOfDay $ time

-- TIMESTAMP

instance FromSQL LocalTime where
  type PQBase LocalTime = PGtimestamp
  fromSQL Nothing = unexpectedNULL
  fromSQL (Just PGtimestamp{..}) = return $ LocalTime day tod
    where
      day = pgDateToDay pgTimestampDate
      tod = pgTimeToTimeOfDay pgTimestampTime

-- TIMESTAMPTZ

instance FromSQL UTCTime where
  type PQBase UTCTime = PGtimestamp
  fromSQL = localToZoned localTimeToUTC

instance FromSQL ZonedTime where
  type PQBase ZonedTime = PGtimestamp
  fromSQL = localToZoned (flip ZonedTime)

----------------------------------------

pgDateToDay :: PGdate -> Day
pgDateToDay PGdate{..} = fromGregorian year mon mday
  where
    year = adjustBC $ fromIntegral pgDateYear
    mon  = fromIntegral $ pgDateMon + 1
    mday = fromIntegral pgDateMDay

    adjustBC = if pgDateIsBC == 1 then negate . pred else id

pgTimeToTimeOfDay :: PGtime -> TimeOfDay
pgTimeToTimeOfDay PGtime{..} = TimeOfDay hour mins $ sec + fromRational (usec % 1000000)
  where
    hour = fromIntegral pgTimeHour
    mins = fromIntegral pgTimeMin
    sec  = fromIntegral pgTimeSec
    usec = fromIntegral pgTimeUSec

localToZoned :: (TimeZone -> LocalTime -> a) -> Maybe PGtimestamp -> IO a
localToZoned _ Nothing = unexpectedNULL
localToZoned construct jts@(Just PGtimestamp{..}) = do
  localTime <- fromSQL jts
  case rest of
    0 -> return . construct (minutesToTimeZone mins) $ localTime
    _ -> E.throwIO . InternalError $ "Invalid gmtoff: " ++ show gmtoff
  where
    gmtoff = pgTimeGMTOff pgTimestampTime
    (mins, rest) = fromIntegral gmtoff `divMod` 60
