module Database.PostgreSQL.PQTypes.Interval
  ( Interval
  , sameComponents
  , iyears
  , imonths
  , idays
  , ihours
  , iminutes
  , iseconds
  , imicroseconds
  ) where

import BinaryParser qualified as BP
import Control.Exception (throw)
import Data.Int
import Data.Semigroup qualified as SG

import Database.PostgreSQL.PQTypes.Format
import Database.PostgreSQL.PQTypes.FromSQL
import Database.PostgreSQL.PQTypes.Internal.Decoding qualified as D
import Database.PostgreSQL.PQTypes.Internal.Encoding qualified as E
import Database.PostgreSQL.PQTypes.Internal.Error
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

-- | Check that two intervals consist of the same components, as opposed to
-- merely being equal.
--
-- 'Eq' compares intervals the way the server's comparison operators do, so
-- it holds for values that the server nonetheless treats differently in
-- arithmetic: @'imonths' 1 == 'idays' 30@, yet adding the former to
-- @2024-01-31@ gives @2024-02-29@ and the latter @2024-03-01@. Use this to
-- tell such values apart, e.g. to check that a roundtrip through the
-- database preserved an interval exactly.
sameComponents :: Interval -> Interval -> Bool
sameComponents a b =
  intMicroseconds a == intMicroseconds b
    && intDays a == intDays b
    && intMonths a == intMonths b

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
    interval usecs days months
    where
      -- The infinities are all three components at the extreme of their
      -- range, which 'Interval' has no values standing for.
      interval :: Int64 -> Int32 -> Int32 -> BP.BinaryParser Interval
      interval usecs days months
        | components == (maxBound, maxBound, maxBound) = unrepresentable "infinity"
        | components == (minBound, minBound, minBound) = unrepresentable "-infinity"
        | otherwise = pure $ mkInterval usecs days months
        where
          components = (usecs, days, months)
          unrepresentable value =
            BP.failure $ "interval '" <> value <> "' cannot be represented by Interval"

instance ToSQL Interval where
  toSQL value@Interval {..}
    -- Such a value would arrive as an infinity, so it's rejected the way the
    -- date and time encoders reject the representations of theirs.
    | components == (maxBound, maxBound, maxBound) = outOfRange
    | components == (minBound, minBound, minBound) = outOfRange
    | otherwise =
        Just $
          mconcat
            [ E.int8_int64 intMicroseconds
            , E.int4_int32 intDays
            , E.int4_int32 intMonths
            ]
    where
      components = (intMicroseconds, intDays, intMonths)

      outOfRange :: a
      outOfRange =
        throw . HPQTypesError $
          "interval: "
            ++ show value
            ++ " is outside the range representable by the wire format"

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
