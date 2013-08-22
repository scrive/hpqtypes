{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, FunctionalDependencies, OverloadedStrings
  , RecordWildCards, ScopedTypeVariables, UndecidableInstances #-}
module DB.FromSQL (FromSQL(..)) where

import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import Data.Int
import Data.Ratio
import Data.Text (Text)
import Data.Text.Encoding
import Data.Time
import Foreign.C
import Foreign.Storable
import qualified Data.ByteString as BS

import DB.Primitive.Types

class Storable base => FromSQL base dest | dest -> base where
  pqTypesFormat :: dest -> ByteString
  fromSQL       :: Bool -> base -> IO dest

instance FromSQL base dest => FromSQL base (Maybe dest) where
  pqTypesFormat _ = pqTypesFormat (undefined::dest)
  fromSQL True _ = return Nothing
  fromSQL False src = Just <$> fromSQL False src

instance FromSQL CInt Int32 where
  pqTypesFormat _ = "%int4"
  fromSQL _ = return . fromIntegral

instance FromSQL PGtimestamp LocalTime where
  pqTypesFormat _ = "%timestamp"
  fromSQL _ PGtimestamp{..} = return $ LocalTime day tod
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
  pqTypesFormat _ = "%timestamptz"
  fromSQL isNull ts@PGtimestamp{..} = do
    localTime <- fromSQL isNull ts
    when (rest /= 0) $
      error "FromSQL PGtimestamp UTCTime: wtf?"
    return $ localTimeToUTC (minutesToTimeZone mins) localTime
    where
      (mins, rest) = fromIntegral (pgTimeGMTOff pgTimestampTime) `divMod` 60

instance FromSQL CString ByteString where
  pqTypesFormat _ = "%text"
  fromSQL _ = BS.packCString

instance FromSQL CString Text where
  pqTypesFormat _ = "%text"
  fromSQL _ cs = decodeUtf8 <$> BS.packCString cs

------------

instance FromSQL CInt CInt where
  pqTypesFormat _ = "%int4"
  fromSQL _ = return

instance FromSQL PGtimestamp PGtimestamp where
  pqTypesFormat _ = "%timestamp"
  fromSQL _ = return

instance FromSQL CString CString where
  pqTypesFormat _ = "%text"
  fromSQL _ = return
