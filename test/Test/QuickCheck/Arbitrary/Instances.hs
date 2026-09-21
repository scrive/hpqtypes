{-# OPTIONS_GHC -Wno-orphans #-}

module Test.QuickCheck.Arbitrary.Instances where

import Data.Aeson
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Char
import Data.IP
import Data.Int
import Data.Scientific
import Data.Text qualified as T
import Data.Time
import Data.UUID.Types qualified as U
import Data.Vector qualified as V
import Data.Word
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Database.PostgreSQL.PQTypes

-- | 'String' with a generator restricted to Latin-1 characters other than
-- NUL. PostgreSQL rejects NUL in text values.
newtype String0 = String0 {unString0 :: String}
  deriving stock (Eq, Ord, Show)
  deriving newtype (PQFormat, ToSQL, FromSQL)

instance Arbitrary String0 where
  arbitrary = String0 . map (chr . fromIntegral . unWord0) <$> arbitrary

-- | 'Word8' with a generator that excludes 0. Textual values built from
-- these bytes ('String0', 'T.Text') therefore contain no NUL characters.
-- PostgreSQL rejects NUL in text values.
newtype Word0 = Word0 {unWord0 :: Word8}
  deriving newtype (Enum, Eq, Integral, Num, Ord, Real)

instance Bounded Word0 where
  minBound = 1
  maxBound = 255

instance Arbitrary Word0 where
  arbitrary = arbitrarySizedBoundedIntegral
  shrink = shrinkIntegral

newtype AsciiChar = AsciiChar {unAsciiChar :: Char}
  deriving stock (Eq, Show)
  deriving newtype (PQFormat, ToSQL, FromSQL)

instance Arbitrary AsciiChar where
  -- QuickCheck >= 2.10 generates Unicode characters for Char, but PostgreSQL
  -- only accepts ASCII ones here. Unlike in text values, NUL is fine: a "char"
  -- value is a raw byte.
  arbitrary = AsciiChar . chr <$> oneof [choose (0, 127), choose (0, 255)]
  shrink = map AsciiChar . shrink . unAsciiChar

instance Arbitrary BS.ByteString where
  arbitrary = BS.pack . map unWord0 <$> arbitrary

instance Arbitrary T.Text where
  arbitrary = T.pack . unString0 <$> arbitrary

uuidFromWords :: (Word32, Word32, Word32, Word32) -> U.UUID
uuidFromWords (a, b, c, d) = U.fromWords a b c d

instance Arbitrary U.UUID where
  arbitrary = uuidFromWords <$> arbitrary
  shrink = map uuidFromWords . shrink . U.toWords

----------------------------------------

-- | The three components of an interval are mutually independent and
-- signed. The generator therefore draws them independently, with mixed signs
-- and values past the carry boundaries (e.g. more than 24 hours worth of
-- microseconds).
instance Arbitrary Interval where
  arbitrary =
    mconcat
      <$> sequence
        [ imonths <$> choose (-1000, 1000)
        , idays <$> choose (-1000, 1000)
        , imicroseconds <$> choose (-172800000000, 172800000000) -- 2 days
        ]

-- | 'Interval' with structural equality. 'Eq' of 'Interval' compares the way
-- the server's operators do. A roundtrip test that uses it can't see a
-- change of components that preserves the estimate, e.g. days folded into
-- microseconds. The server treats such values differently in arithmetic.
newtype Interval0 = Interval0 Interval
  deriving newtype (Arbitrary, FromSQL, PQFormat, Show, ToSQL)

instance Eq Interval0 where
  Interval0 a == Interval0 b = sameComponents a b

instance Arbitrary json => Arbitrary (JSON json) where
  arbitrary = JSON <$> arbitrary

instance Arbitrary jsonb => Arbitrary (JSONB jsonb) where
  arbitrary = JSONB <$> arbitrary

instance Arbitrary a => Arbitrary (V.Vector a) where
  arbitrary = V.fromList <$> arbitrary

-- | Two-dimensional array with a generator that respects the rectangularity
-- of PostgreSQL arrays.
newtype Matrix a = Matrix [[a]]
  deriving stock (Show)
  deriving newtype (PQFormat, ToSQL, FromSQL)

-- | The server normalizes arrays with no elements to zero-dimensional ones.
-- A matrix of empty rows is therefore equal to the empty matrix.
instance Eq a => Eq (Matrix a) where
  Matrix [] == Matrix yss = all null yss
  Matrix xss == Matrix [] = all null xss
  Matrix xss == Matrix yss = xss == yss

instance Arbitrary a => Arbitrary (Matrix a) where
  arbitrary = do
    let bound = (`mod` 100) . abs
    outerDim <- bound <$> arbitrary
    innerDim <- bound <$> arbitrary
    Matrix <$> vectorOf outerDim (vectorOf innerDim arbitrary)

-- | Like 'Matrix', but based on 'V.Vector'.
newtype VMatrix a = VMatrix (V.Vector (V.Vector a))
  deriving stock (Show)
  deriving (PQFormat, ToSQL, FromSQL) via V.Vector (V.Vector a)

instance Eq a => Eq (VMatrix a) where
  VMatrix v1 == VMatrix v2
    | V.null v1 = V.all V.null v2
    | V.null v2 = V.all V.null v1
    | otherwise = v1 == v2

instance Arbitrary a => Arbitrary (VMatrix a) where
  arbitrary = do
    Matrix xss <- arbitrary
    pure . VMatrix . V.fromList $ map V.fromList xss

----------------------------------------

instance Arbitrary Scientific where
  arbitrary = scientific <$> arbitrary <*> (subtract 50 . (`mod` 100) <$> arbitrary)

-- | 'Value' with a custom generator. The 'Arbitrary' instance from @aeson@ is
-- unsuitable for roundtrip tests. Its strings and object keys can contain NUL
-- characters, which PostgreSQL rejects. Its numbers have unbounded exponents,
-- which explode in a @numeric@ column.
newtype Value0 = Value0 {unValue0 :: Value}
  deriving newtype (Eq, FromJSON, Show, ToJSON)

instance Arbitrary Value0 where
  arbitrary = Value0 <$> value depth depth
    where
      depth :: Int
      depth = 3

      value !i !n
        | i == 0 = oneof leafs
        | i == n = oneof branches
        | otherwise = oneof $ leafs ++ branches
        where
          branches =
            [ Object . KM.fromList
                <$> shortListOf ((,) . K.fromText <$> arbitrary <*> subValue)
            , Array . V.fromList <$> shortListOf subValue
            ]
          leafs =
            [ String <$> arbitrary
            , Number <$> arbitrary
            , Bool <$> arbitrary
            , pure Null
            ]

          subValue = value (i - 1) n
          shortListOf = fmap (take depth) . listOf

-- | 'Value0' with the SQL instances derived via t'JSONB'.
newtype JSONBValue0 = JSONBValue0 Value0
  deriving newtype (Arbitrary, Eq, Show)
  deriving (PQFormat, ToSQL, FromSQL) via JSONB Value0

-- | The @json@ type stores its input text verbatim. A raw value therefore
-- roundtrips if the generator produces the encoding of a JSON value. The
-- server rejects anything else.
instance Arbitrary RawJSON where
  arbitrary = RawJSON . BSL.toStrict . encode <$> arbitrary @Value0

----------------------------------------

instance Arbitrary Day where
  arbitrary = ModifiedJulianDay <$> arbitrary

-- | Generate a duration of a whole number of microseconds, less than the
-- given number of seconds. Time related values have microsecond precision,
-- because that's the precision of their binary wire format. Haskell's time
-- types can hold sub-microsecond digits, but the server returns such values
-- rounded to whole microseconds. If the generator produced them, they would
-- not roundtrip exactly and the tests would need approximate comparison.
microseconds :: Fractional a => Int64 -> Gen a
microseconds secs = (/ 1000000) . fromIntegral <$> choose (0, secs * 1000000 - 1)

instance Arbitrary TimeOfDay where
  arbitrary = do
    hours <- choose (0, 23)
    mins <- choose (0, 59)
    secs <- microseconds 60
    pure $ TimeOfDay hours mins secs

instance Arbitrary LocalTime where
  arbitrary = LocalTime <$> arbitrary <*> arbitrary

instance Arbitrary UTCTime where
  arbitrary = do
    day <- arbitrary
    -- The day time stays below 86400 seconds, because the server normalizes
    -- a larger value into the next day.
    secs <- microseconds 86400
    pure $ UTCTime day secs

----------------------------------------

instance Arbitrary IP where
  arbitrary = oneof [IPv4 <$> genIPv4, IPv6 <$> genIPv6]

instance Arbitrary IPRange where
  arbitrary =
    oneof
      [ IPv4Range <$> (makeAddrRange <$> genIPv4 <*> choose (0, 32))
      , IPv6Range <$> (makeAddrRange <$> genIPv6 <*> choose (0, 128))
      ]

genIPv4 :: Gen IPv4
genIPv4 = toIPv4 <$> vectorOf 4 (choose (0, 255))

genIPv6 :: Gen IPv6
genIPv6 = toIPv6 <$> vectorOf 8 (choose (0, 65535))

----------------------------------------

-- | Range of a discrete type in the canonical form: inclusive lower bound,
-- exclusive upper bound. The server normalizes such ranges to this form.
discreteRange :: (Arbitrary a, Ord a) => Gen (Range a)
discreteRange =
  frequency
    [ (1, pure Empty)
    ,
      ( 9
      , do
          (a, b) <- arbitrary `suchThat` uncurry (/=)
          lower <- elements [Incl $ min a b, Inf]
          upper <- elements [Excl $ max a b, Inf]
          pure $ Range lower upper
      )
    ]

-- | Range of a continuous type. The bounds never have an equal value,
-- because the server normalizes such a range to 'Empty' unless both bounds
-- are inclusive.
continuousRange :: (Arbitrary a, Ord a) => Gen (Range a)
continuousRange =
  frequency
    [ (1, pure Empty)
    ,
      ( 9
      , do
          (a, b) <- arbitrary `suchThat` uncurry (/=)
          lower <- elements [Incl $ min a b, Excl $ min a b, Inf]
          upper <- elements [Incl $ max a b, Excl $ max a b, Inf]
          pure $ Range lower upper
      )
    ]

instance Arbitrary (Range Int32) where
  arbitrary = discreteRange

instance Arbitrary (Range Int64) where
  arbitrary = discreteRange

instance Arbitrary (Range Day) where
  arbitrary = discreteRange

instance Arbitrary (Range Scientific) where
  arbitrary = continuousRange

instance Arbitrary (Range LocalTime) where
  arbitrary = continuousRange

instance Arbitrary (Range UTCTime) where
  arbitrary = continuousRange
