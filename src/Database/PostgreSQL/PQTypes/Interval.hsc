module Database.PostgreSQL.PQTypes.Interval
  ( Interval(..)
  , iyears
  , imonths
  , idays
  , ihours
  , iminutes
  , iseconds
  , imicroseconds
  ) where

import Data.Int
import Data.List
import Foreign.Storable
import Data.ByteString.Char8 qualified as BS
import Data.Semigroup qualified as SG

import Database.PostgreSQL.PQTypes.Format
import Database.PostgreSQL.PQTypes.FromSQL
import Database.PostgreSQL.PQTypes.Internal.Utils
import Database.PostgreSQL.PQTypes.ToSQL

#include <libpqtypes.h>

----------------------------------------

-- | Representation of INTERVAL PostgreSQL type.
data Interval = Interval
  { intYears         :: !Int32
  , intMonths        :: !Int32
  , intDays          :: !Int32
  , intHours         :: !Int32
  , intMinutes       :: !Int32
  , intSeconds       :: !Int32
  , intMicroseconds  :: !Int32
  } deriving (Eq, Ord)

instance Show Interval where
  showsPrec _ Interval{..} = (++) . intercalate ", " $ filter (not . null)
    [ f intYears "year"
    , f intMonths "month"
    , f intDays "day"
    , f intHours "hour"
    , f intMinutes "minute"
    , f intSeconds "second"
    , f intMicroseconds "microsecond"
    ]
    where
      f n desc = case n of
        0 -> ""
        1 -> show n ++ " " ++ desc
        _ -> show n ++ " " ++ desc ++ "s"

instance SG.Semigroup Interval where
  a <> b = Interval
    { intYears = intYears a + intYears b
    , intMonths = intMonths a + intMonths b
    , intDays = intDays a + intDays b
    , intHours = intHours a + intHours b
    , intMinutes = intMinutes a + intMinutes b
    , intSeconds = intSeconds a + intSeconds b
    , intMicroseconds = intMicroseconds a + intMicroseconds b
    }

instance Monoid Interval where
  mempty  = Interval 0 0 0 0 0 0 0
  mappend = (SG.<>)

instance Storable Interval where
  sizeOf _ = #{size PGinterval}
  alignment _ = #{alignment PGinterval}
  peek ptr = Interval
    <$> #{peek PGinterval, years} ptr
    <*> #{peek PGinterval, mons} ptr
    <*> #{peek PGinterval, days} ptr
    <*> #{peek PGinterval, hours} ptr
    <*> #{peek PGinterval, mins} ptr
    <*> #{peek PGinterval, secs} ptr
    <*> #{peek PGinterval, usecs} ptr
  poke ptr Interval{..} = do
    #{poke PGinterval, years} ptr intYears
    #{poke PGinterval, mons} ptr intMonths
    #{poke PGinterval, days} ptr intDays
    #{poke PGinterval, hours} ptr intHours
    #{poke PGinterval, mins} ptr intMinutes
    #{poke PGinterval, secs} ptr intSeconds
    #{poke PGinterval, usecs} ptr intMicroseconds

instance PQFormat Interval where
  pqFormat = BS.pack "%interval"

instance FromSQL Interval where
  type PQBase Interval = Interval
  fromSQL Nothing = unexpectedNULL
  fromSQL (Just int) = pure int

instance ToSQL Interval where
  type PQDest Interval = Interval
  toSQL int _ = putAsPtr int

----------------------------------------

-- | Convert 'Int32' to appropriate 'Interval'
-- representation of given number of years.
iyears :: Int32 -> Interval
iyears v = mempty { intYears = v }

-- | Convert 'Int32' to appropriate 'Interval'
-- representation of given number of months.
imonths :: Int32 -> Interval
imonths v = mempty { intMonths = v }

-- | Convert 'Int32' to appropriate 'Interval'
-- representation of given number of days.
idays :: Int32 -> Interval
idays v = mempty { intDays = v }

-- | Convert 'Int32' to appropriate 'Interval'
-- representation of given number of hours.
ihours :: Int32 -> Interval
ihours v = mempty { intHours = v }

-- | Convert 'Int32' to appropriate 'Interval'
-- representation of given number of minutes.
iminutes :: Int32 -> Interval
iminutes v = mempty { intMinutes = v }

-- | Convert 'Int32' to appropriate 'Interval'
-- representation of given number of seconds.
iseconds :: Int32 -> Interval
iseconds v = mempty { intSeconds = v }

-- | Convert 'Int32' to appropriate 'Interval'
-- representation of given number of microseconds.
imicroseconds :: Int32 -> Interval
imicroseconds v = mempty { intMicroseconds = v }
