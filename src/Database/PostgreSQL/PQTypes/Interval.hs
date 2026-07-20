module Database.PostgreSQL.PQTypes.Interval
  ( Interval
  , iyears
  , imonths
  , idays
  , ihours
  , iminutes
  , iseconds
  , imicroseconds
  ) where

import BinaryParser qualified as BP
import Data.Int
import Data.Semigroup qualified as SG
import PostgreSQL.Binary.Decoding qualified as D
import PostgreSQL.Binary.Encoding qualified as E

import Database.PostgreSQL.PQTypes.Format
import Database.PostgreSQL.PQTypes.FromSQL
import Database.PostgreSQL.PQTypes.Internal.Oid
import Database.PostgreSQL.PQTypes.ToSQL

----------------------------------------

-- | Representation of the INTERVAL PostgreSQL type: like on the wire, it
-- consists of three components of mutually independent duration (a month
-- has no fixed number of days and a day no fixed number of hours).
--
-- The type is opaque; construct values with the 'iyears', 'imonths' etc.
-- functions combined via the 'Monoid' instance, e.g. @idays 2 <> ihours 3@.
data Interval = Interval
  { intMicroseconds :: !Int64
  , intDays :: !Int32
  , intMonths :: !Int32
  , intEstimate :: !Integer
  -- ^ Cached estimate the server's comparison of intervals is based on
  -- (months converted at 30 days, days at 24 hours), used by 'Eq' and
  -- 'Ord'. Always construct through 'mkInterval' to keep it in sync with
  -- the components.
  }

-- | Smart constructor computing the cached estimate.
mkInterval :: Int64 -> Int32 -> Int32 -> Interval
mkInterval usecs days months =
  Interval
    { intMicroseconds = usecs
    , intDays = days
    , intMonths = months
    , intEstimate =
        (fromIntegral months * 30 + fromIntegral days) * 86400000000
          + fromIntegral usecs
    }

-- | Compares the same way the server's comparison operators do, i.e. by the
-- estimate with months converted at 30 days and days at 24 hours. In
-- particular @imonths 1 == idays 30@ holds, just like @1 month = 30 days@
-- does on the server, even though the values are distinct (e.g. adding them
-- to a timestamp can give different results).
instance Eq Interval where
  a == b = intEstimate a == intEstimate b

-- | See the 'Eq' instance.
instance Ord Interval where
  compare a b = compare (intEstimate a) (intEstimate b)

-- | Shows the components of the wire format (the cached estimate is
-- omitted).
instance Show Interval where
  showsPrec d Interval {..} =
    showParen (d > 10) $
      showString "Interval {intMicroseconds = "
        . shows intMicroseconds
        . showString ", intDays = "
        . shows intDays
        . showString ", intMonths = "
        . shows intMonths
        . showString "}"

instance SG.Semigroup Interval where
  a <> b =
    mkInterval
      (intMicroseconds a + intMicroseconds b)
      (intDays a + intDays b)
      (intMonths a + intMonths b)

instance Monoid Interval where
  mempty = mkInterval 0 0 0
  mappend = (SG.<>)

instance PQFormat Interval where
  pqOid = intervalOid
  pqArrayOid = intervalArrayOid

-- The binary wire format of @interval@ is (microseconds :: int64,
-- days :: int32, months :: int32), which 'Interval' mirrors directly; the
-- (de)serialization functions of postgresql-binary are of no use here as
-- 'Data.Time.DiffTime' cannot represent days and months separately.

instance FromSQL Interval where
  fromSQL = decodeScalar . D.fn . BP.run $ do
    usecs :: Int64 <- fromIntegral <$> BP.beWord64
    days :: Int32 <- fromIntegral <$> BP.beWord32
    months :: Int32 <- fromIntegral <$> BP.beWord32
    BP.endOfInput
    pure $ mkInterval usecs days months

instance ToSQL Interval where
  toSQL Interval {..} =
    Just $
      mconcat
        [ E.int8_int64 intMicroseconds
        , E.int4_int32 intDays
        , E.int4_int32 intMonths
        ]

----------------------------------------

-- | An interval of the given number of years (a year is 12 months).
iyears :: Int32 -> Interval
iyears v = mkInterval 0 0 (12 * v)

-- | An interval of the given number of months.
imonths :: Int32 -> Interval
imonths v = mkInterval 0 0 v

-- | An interval of the given number of days.
idays :: Int32 -> Interval
idays v = mkInterval 0 v 0

-- | An interval of the given number of hours.
ihours :: Int64 -> Interval
ihours v = mkInterval (v * 3600000000) 0 0

-- | An interval of the given number of minutes.
iminutes :: Int64 -> Interval
iminutes v = mkInterval (v * 60000000) 0 0

-- | An interval of the given number of seconds.
iseconds :: Int64 -> Interval
iseconds v = mkInterval (v * 1000000) 0 0

-- | An interval of the given number of microseconds.
imicroseconds :: Int64 -> Interval
imicroseconds v = mkInterval v 0 0
