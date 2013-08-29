{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, FunctionalDependencies, RecordWildCards
  , ScopedTypeVariables, UndecidableInstances #-}
module DB.FromSQL (FromSQL(..)) where

import Control.Applicative
import Control.Monad
import Data.Int
import Data.Ratio
import Data.Text (Text)
import Data.Text.Encoding
import Data.Time
import Foreign.C
import Foreign.Storable
import qualified Data.ByteString.Char8 as BS

import DB.Primitive.Types

errorNull :: IO a
errorNull = error "unexpected NULL value"

class Storable base => FromSQL base dest | dest -> base where
  pqTypesFormat :: dest -> BS.ByteString
  fromSQL       :: Maybe base -> IO dest

instance FromSQL base dest => FromSQL base (Maybe dest) where
  pqTypesFormat _ = pqTypesFormat (undefined::dest)
  fromSQL mbase = case mbase of
    Just _  -> Just <$> fromSQL mbase
    Nothing -> return Nothing

instance FromSQL CInt Int32 where
  pqTypesFormat _ = BS.pack "%int4"

  fromSQL Nothing = errorNull
  fromSQL (Just n) = return $ fromIntegral n

instance FromSQL PGtimestamp LocalTime where
  pqTypesFormat _ = BS.pack "%timestamp"

  fromSQL Nothing = errorNull
  fromSQL (Just PGtimestamp{..}) = return $ LocalTime day tod
    where
      day = fromGregorian year mon mday
      tod = TimeOfDay hour mins $ sec + fromRational (usec % 1000000)

      year = fromIntegral $ pgDateYear pgTimestampDate
      mon  = fromIntegral $ pgDateMon pgTimestampDate + 1
      mday = fromIntegral $ pgDateMDay pgTimestampDate
      hour = fromIntegral $ pgTimeHour pgTimestampTime
      mins = fromIntegral $ pgTimeMin pgTimestampTime
      sec  = fromIntegral $ pgTimeSec pgTimestampTime
      usec = fromIntegral $ pgTimeUSec pgTimestampTime

instance FromSQL PGtimestamp UTCTime where
  pqTypesFormat _ = BS.pack "%timestamptz"

  fromSQL Nothing = errorNull
  fromSQL (Just ts@PGtimestamp{..}) = do
    localTime <- fromSQL $ Just ts
    when (rest /= 0) $
      error "FromSQL PGtimestamp UTCTime: invalid gmtoff"
    return $ localTimeToUTC (minutesToTimeZone mins) localTime
    where
      (mins, rest) = fromIntegral (pgTimeGMTOff pgTimestampTime) `divMod` 60

instance FromSQL CString BS.ByteString where
  pqTypesFormat _ = BS.pack "%text"

  fromSQL Nothing = errorNull
  fromSQL (Just cs) = BS.packCString cs

instance FromSQL CString Text where
  pqTypesFormat _ = BS.pack "%text"

  fromSQL Nothing = errorNull
  fromSQL (Just cs) = decodeUtf8 <$> BS.packCString cs

instance FromSQL CString String where
  pqTypesFormat _ = BS.pack "%text"

  fromSQL Nothing = errorNull
  fromSQL (Just cs) = peekCString cs
