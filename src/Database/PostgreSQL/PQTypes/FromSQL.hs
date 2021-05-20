module Database.PostgreSQL.PQTypes.FromSQL (
    FromSQL(..)
  ) where

import Data.Int
import Data.Kind (Type)
import Data.Ratio
import Data.Text.Encoding
import Data.Time
import Data.Word
import Foreign.C
import Foreign.Storable
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.UUID.Types as U

import Database.PostgreSQL.PQTypes.Format
import Database.PostgreSQL.PQTypes.Internal.C.Types
import Database.PostgreSQL.PQTypes.Internal.Utils

-- | Class which represents \"from SQL (libpqtypes)
-- type to Haskell type\" transformation.
class (PQFormat t, Storable (PQBase t)) => FromSQL t where
  -- | Base type (used by libpqtypes).
  type PQBase t :: Type
  -- | Convert value of base type to target one.
  fromSQL :: Maybe (PQBase t) -- ^ base value (Nothing if NULL was delivered)
          -> IO t

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

-- | Assumes that source C string is UTF-8, so if you are working
-- with a different encoding, you should not rely on this instance.
instance FromSQL T.Text where
  type PQBase T.Text = PGbytea
  fromSQL mbytea = either E.throwIO return . decodeUtf8' =<< fromSQL mbytea

-- | Assumes that source C string is UTF-8, so if you are working
-- with a different encoding, you should not rely on this instance
instance FromSQL TL.Text where
  type PQBase TL.Text = PGbytea
  fromSQL = fmap TL.fromStrict . fromSQL

-- | Assumes that source C string is UTF-8, so if you are working
-- with a different encoding, you should not rely on this instance.
instance FromSQL String where
  type PQBase String = PGbytea
  fromSQL mbytea = T.unpack <$> fromSQL mbytea

instance FromSQL U.UUID where
  type PQBase U.UUID = PGuuid
  fromSQL Nothing = unexpectedNULL
  fromSQL (Just (PGuuid w1 w2 w3 w4)) = return $ U.fromWords w1 w2 w3 w4

-- BYTEA

instance FromSQL BS.ByteString where
  type PQBase BS.ByteString = PGbytea
  fromSQL Nothing = unexpectedNULL
  fromSQL (Just bytea) = BS.packCStringLen $ byteaToCStringLen bytea

instance FromSQL BSL.ByteString where
  type PQBase BSL.ByteString = PGbytea
  fromSQL = fmap BSL.fromStrict . fromSQL

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

-- | 'FromSQL' instance for 'ZonedTime' doesn't exist because
-- PostgreSQL doesn't provide zone offset information when returning
-- timestamps with time zone in a binary format.
instance FromSQL UTCTime where
  type PQBase UTCTime = PGtimestamp
  fromSQL Nothing = unexpectedNULL
  fromSQL jts@(Just PGtimestamp{..}) = do
    localTime <- fromSQL jts
    case rest of
      0 -> return . localTimeToUTC (minutesToTimeZone mins) $ localTime
      _ -> hpqTypesError $ "Invalid gmtoff: " ++ show gmtoff
    where
      gmtoff = pgTimeGMTOff pgTimestampTime
      (mins, rest) = fromIntegral gmtoff `divMod` 60

-- BOOL

instance FromSQL Bool where
  type PQBase Bool = CInt
  fromSQL Nothing = unexpectedNULL
  fromSQL (Just n) = case n of
    0 -> return False
    _ -> return True

----------------------------------------

-- | Convert PGtime to Day.
pgDateToDay :: PGdate -> Day
pgDateToDay PGdate{..} = fromGregorian year mon mday
  where
    year = adjustBC $ fromIntegral pgDateYear
    -- Note: libpqtypes represents months as numbers in range
    -- [0, 11], whereas Haskell uses [1, 12], hence plus one.
    mon  = fromIntegral $ pgDateMon + 1
    mday = fromIntegral pgDateMDay
    -- Note: PostgreSQL has no notion of '0th year', it's 1 AD
    -- and then before that 1 BC for it. Since Haskell represents
    -- date according to ISO-8601, where 0th year means 1 BC, we
    -- want to change the sign and adjust the year by one here,
    -- if appropriate.
    adjustBC = if pgDateIsBC == 1 then negate . pred else id

-- | Convert PGtime to TimeOfDay.
pgTimeToTimeOfDay :: PGtime -> TimeOfDay
pgTimeToTimeOfDay PGtime{..} = TimeOfDay hour mins $ sec + fromRational (usec % 1000000)
  where
    hour = fromIntegral pgTimeHour
    mins = fromIntegral pgTimeMin
    sec  = fromIntegral pgTimeSec
    usec = fromIntegral pgTimeUSec
