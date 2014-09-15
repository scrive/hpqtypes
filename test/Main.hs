{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleContexts
  , GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings
  , ScopedTypeVariables, TypeFamilies, UndecidableInstances #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.State
import Data.Char
import Data.Int
import Data.Time
import Data.Typeable
import Data.Word
import System.Environment
import System.Exit
import System.Random
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test, assertEqual)
import Test.QuickCheck
import Test.QuickCheck.Gen
import qualified Control.Exception.Lifted as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T

import Data.Monoid.Space
import Data.Monoid.Utils
import Database.PostgreSQL.PQTypes
import Prelude.Instances ()
import Test.QuickCheck.Arbitrary.Instances ()

type InnerTestEnv = StateT StdGen (DBT IO)

newtype TestEnv a = TestEnv { unTestEnv :: InnerTestEnv a }
  deriving (Applicative, Functor, Monad, MonadBase IO, MonadDB)

instance MonadBaseControl IO TestEnv where
  newtype StM TestEnv a = StTestEnv { unStTestEnv :: StM InnerTestEnv a }
  liftBaseWith f = TestEnv $ liftBaseWith $ \run ->
                         f $ liftM StTestEnv . run . unTestEnv
  restoreM = TestEnv . restoreM . unStTestEnv

withStdGen :: (StdGen -> r) -> TestEnv r
withStdGen f = do
  gen <- TestEnv get
  TestEnv . modify $ snd . next
  return (f gen)

----------------------------------------

type TestData = (StdGen, ConnectionSource)

runTestEnv :: TestData -> TransactionSettings -> TestEnv a -> IO a
runTestEnv (env, cs) ts m = runDBT cs ts $ evalStateT (unTestEnv m) env

runTimes :: Monad m => Int -> m () -> m ()
runTimes !n m = case n of
  0 -> return ()
  _ -> m >> runTimes (n-1) m

----------------------------------------

newtype String0 = String0 { unString0 :: String }
  deriving (Eq, Ord, Show, Typeable, PQFormat)

instance FromSQL String0 where
  type PQBase String0 = PQBase String
  fromSQL = fmap String0 . fromSQL

instance ToSQL String0 where
  type PQDest String0 = PQDest String
  toSQL (String0 s) = toSQL s

instance Arbitrary String0 where
  arbitrary = String0 . map (chr . fromIntegral . unWord0) <$> arbitrary

newtype Word0 = Word0 { unWord0 :: Word8 }
  deriving (Enum, Eq, Integral, Num, Ord, Real)

instance Bounded Word0 where
  minBound = 1
  maxBound = 255

instance Arbitrary Word0 where
  arbitrary = arbitrarySizedBoundedIntegral
  shrink = shrinkIntegral

instance Arbitrary BS.ByteString where
  arbitrary = BS.pack . map unWord0 <$> arbitrary

instance Arbitrary T.Text where
  arbitrary = T.pack . unString0 <$> arbitrary

instance Arbitrary Interval where
  arbitrary = Interval
    <$> abs `fmap` arbitrary
    <*> choose (0, 11)
    <*> choose (0, 364)
    <*> choose (0, 23)
    <*> choose (0, 59)
    <*> choose (0, 59)
    <*> choose (0, 999999)

instance Arbitrary Day where
  arbitrary = ModifiedJulianDay <$> arbitrary

instance Arbitrary TimeOfDay where
  arbitrary = do
    hours <- choose (0, 23)
    mins <- choose (0, 59)
    secs :: Double <- choose (0, 60)
    return $ TimeOfDay hours mins (realToFrac secs)

instance Arbitrary LocalTime where
  arbitrary = LocalTime <$> arbitrary <*> arbitrary

instance Arbitrary UTCTime where
  arbitrary = do
    day <- arbitrary
    secs :: Double <- choose (0, 86401)
    return $ UTCTime day (realToFrac secs)

instance Arbitrary TimeZone where
  arbitrary = elements $ map hoursToTimeZone [-12..14]

instance Arbitrary ZonedTime where
  arbitrary = ZonedTime <$> arbitrary <*> arbitrary

----------------------------------------

instance Arbitrary a => Arbitrary (Binary a) where
  arbitrary = Binary <$> arbitrary

instance Arbitrary a => Arbitrary (Composite a) where
  arbitrary = Composite <$> arbitrary

instance Arbitrary a => Arbitrary (Single a) where
  arbitrary = Single <$> arbitrary

instance Arbitrary a => Arbitrary (Array1 a) where
  arbitrary = arbitraryArray1 Array1

instance Arbitrary a => Arbitrary (CompositeArray1 a) where
  arbitrary = arbitraryArray1 CompositeArray1

instance Arbitrary a => Arbitrary (Array2 a) where
  arbitrary = arbitraryArray2 Array2

instance Arbitrary a => Arbitrary (CompositeArray2 a) where
  arbitrary = arbitraryArray2 CompositeArray2

arbitraryArray1 :: Arbitrary a => (a -> b) -> Gen b
arbitraryArray1 arr1 = arr1 <$> arbitrary

arbitraryArray2 :: Arbitrary a => ([[a]] -> b) -> Gen b
arbitraryArray2 arr2 = do
  let bound = (`mod` 100) . abs
  outerDim <- bound <$> arbitrary
  innerDim <- bound <$> arbitrary
  arr2 <$> vectorOf outerDim (vectorOf innerDim arbitrary)

----------------------------------------

data Simple = Simple (Maybe Int32) (Maybe Day)
  deriving (Eq, Ord, Show, Typeable)

type instance CompositeRow Simple = (Maybe Int32, Maybe Day)

instance PQFormat Simple where
  pqFormat _ = "%simple_"
instance CompositeFromSQL Simple where
  toComposite (a, b) = return $ Simple a b
instance CompositeToSQL Simple where
  fromComposite (Simple a b) = return (a, b)

instance Arbitrary Simple where
  arbitrary = Simple <$> arbitrary <*> arbitrary

data Nested = Nested (Maybe Double) (Maybe Simple)
  deriving (Eq, Ord, Show, Typeable)

type instance CompositeRow Nested = (Maybe Double, Maybe (Composite Simple))

instance PQFormat Nested where
  pqFormat _ = "%nested_"
instance CompositeFromSQL Nested where
  toComposite (a, b) = return $ Nested a (unComposite <$> b)
instance CompositeToSQL Nested where
  fromComposite (Nested a b) = return (a, Composite <$> b)

instance Arbitrary Nested where
  arbitrary = Nested <$> arbitrary <*> arbitrary

----------------------------------------

epsilon :: Fractional a => a
epsilon = 0.00001

eqTOD :: TimeOfDay -> TimeOfDay -> Bool
eqTOD a b = and [
    todHour a == todHour b
  , todMin a == todMin b
  , abs (todSec a - todSec b) < epsilon
  ]

eqLT :: LocalTime -> LocalTime -> Bool
eqLT a b = and [
    localDay a == localDay b
  , localTimeOfDay a `eqTOD` localTimeOfDay b
  ]

eqUTCT :: UTCTime -> UTCTime -> Bool
eqUTCT a b = and [
    utctDay a == utctDay b
  , abs (utctDayTime a - utctDayTime b) < epsilon
  ]

eqZT :: ZonedTime -> ZonedTime -> Bool
eqZT a b = zonedTimeToUTC a `eqUTCT` zonedTimeToUTC b

eqArray2 :: Eq a => Array2 a -> Array2 a -> Bool
eqArray2 (Array2 []) (Array2 arr) = all null arr
eqArray2 (Array2 arr) (Array2 []) = all null arr
eqArray2 a b = a == b

eqCompositeArray2 :: Eq a => CompositeArray2 a -> CompositeArray2 a -> Bool
eqCompositeArray2 (CompositeArray2 []) (CompositeArray2 arr) = all null arr
eqCompositeArray2 (CompositeArray2 arr) (CompositeArray2 []) = all null arr
eqCompositeArray2 a b = a == b

----------------------------------------

dts :: TransactionSettings
dts = defaultTransactionSettings

randomValue :: Arbitrary t => Int -> TestEnv t
randomValue n = withStdGen $ \gen -> unGen arbitrary gen n

assertEqual :: (Show a, MonadBase IO m)
            => String -> a -> a -> (a -> a -> Bool) -> m ()
assertEqual preface expected actual eq =
  liftBase $ unless (actual `eq` expected) (assertFailure msg)
  where
    msg = concat [
        if null preface then "" else preface ++ "\n"
      , "expected: " ++ show expected ++ "\n but got: " ++ show actual
      ]

----------------------------------------

autocommitTest :: TestData -> Test
autocommitTest td = testCase "Autocommit mode works" . runTestEnv td dts{tsAutoTransaction = False} $ do
  let sint = Single (1::Int32)
  runQuery_ $ rawSQL "INSERT INTO test1_ (a) VALUES ($1)" sint
  withNewConnection $ do
    n <- runQuery $ rawSQL "SELECT a FROM test1_ WHERE a = $1" sint
    assertEqual "Other connection sees autocommited data" n 1 (==)
  runQuery_ $ rawSQL "DELETE FROM test1_ WHERE a = $1" sint

readOnlyTest :: TestData -> Test
readOnlyTest td = testCase "Read only transaction mode works" . runTestEnv td dts{tsPermissions = ReadOnly} $ do
  let sint = Single (2::Int32)
  eres <- E.try . runQuery_ $ rawSQL "INSERT INTO test1_ (a) VALUES ($1)" sint
  case eres :: Either DBException () of
    Left _ -> return ()
    Right _ -> liftBase . assertFailure $ "DBException wasn't thrown"
  rollback
  n <- runQuery $ rawSQL "SELECT a FROM test1_ WHERE a = $1" sint
  assertEqual "SELECT works in read only mode" n 0 (==)

savepointTest :: TestData -> Test
savepointTest td = testCase "Savepoint support works" . runTestEnv td dts $ do
  let int1 = 3 :: Int32
      int2 = 4 :: Int32

  -- action executed within withSavepoint throws
  runQuery_ $ rawSQL "INSERT INTO test1_ (a) VALUES ($1)" (Single int1)
  _ :: Either DBException () <- E.try . withSavepoint (Savepoint "test") $ do
    runQuery_ $ rawSQL "INSERT INTO test1_ (a) VALUES ($1)" (Single int2)
    runSQL_ "SELECT * FROM table_that_is_not_there"
  runQuery_ $ rawSQL "SELECT a FROM test1_ WHERE a IN ($1, $2)" (int1, int2)
  res1 <- fetchMany unSingle
  assertEqual "Part of transaction was rolled back" res1 [int1] (==)

  rollback

  -- action executed within withSavepoint doesn't throw
  runQuery_ $ rawSQL "INSERT INTO test1_ (a) VALUES ($1)" (Single int1)
  withSavepoint (Savepoint "test") $ do
    runQuery_ $ rawSQL "INSERT INTO test1_ (a) VALUES ($1)" (Single int2)
  runQuery_ $ rawSQL "SELECT a FROM test1_ WHERE a IN ($1, $2) ORDER BY a" (int1, int2)
  res2 <- fetchMany unSingle
  assertEqual "Result of all queries is visible" res2 [int1, int2] (==)

transactionTest :: TestData -> IsolationLevel -> Test
transactionTest td lvl = testCase ("Auto transaction works by default with isolation level" <+> show lvl) . runTestEnv td dts{tsIsolationLevel = lvl} $ do
  let sint = Single (5::Int32)
  runQuery_ $ rawSQL "INSERT INTO test1_ (a) VALUES ($1)" sint
  withNewConnection $ do
    n <- runQuery $ rawSQL "SELECT a FROM test1_ WHERE a = $1" sint
    assertEqual "Other connection doesn't see uncommited data" n 0 (==)
  rollback

nullTest :: forall t. (Show t, ToSQL t, FromSQL t, Typeable t)
         => TestData -> t -> Test
nullTest td t = testCase ("Attempt to get non-NULL value of type" <+> show (typeOf t) <+> "fails if NULL is provided") . runTestEnv td dts $ do
  runSQL_ $ "SELECT" <?> (Nothing::Maybe t)
  eres  <- E.try $ fetchOne unSingle
  case eres :: Either DBException t of
    Left _ -> return ()
    Right _ -> liftBase . assertFailure $ "DBException wasn't thrown"

putGetTest :: forall t. (Arbitrary t, Show t, ToSQL t, FromSQL t, Typeable t)
           => TestData -> Int -> t -> (t -> t -> Bool) -> Test
putGetTest td n t eq = testCase ("Putting value of type" <+> show (typeOf t) <+> "through database doesn't change its value") . runTestEnv td dts . runTimes 1000 $ do
  v :: t <- randomValue n
  --liftBase . putStrLn . show $ v
  runSQL_ $ "SELECT" <?> v
  v' <- fetchOne unSingle
  assertEqual "Value doesn't change after getting through database" v v' eq

rowTest :: forall row. (Arbitrary row, Eq row, Show row, ToRow row, FromRow row)
        => TestData -> row -> Test
rowTest td r = testCase ("Putting row of length" <+> show (pqVariables r) <+> "through database works") . runTestEnv td dts . runTimes 100 $ do
  row :: row <- randomValue 100
  let fmt = mintercalate ", " $ map (BSC.pack . ('$' :) . show) [1..pqVariables r]
  runQuery_ $ rawSQL ("SELECT" <+> fmt) row
  row' <- fetchOne id
  assertEqual "Row doesn't change after getting through database" row row' (==)

_printTime :: MonadBase IO m => m a -> m a
_printTime m = do
  t <- liftBase getCurrentTime
  res <- m
  t' <- liftBase getCurrentTime
  liftBase . putStrLn $ "Time: " ++ show (diffUTCTime t' t)
  return res

tests :: TestData -> [Test]
tests td = [
    autocommitTest td
  , readOnlyTest td
  , savepointTest td
  ----------------------------------------
  , transactionTest td ReadCommitted
  , transactionTest td RepeatableRead
  , transactionTest td Serializable
  ----------------------------------------
  , nullTest td (u::Int16)
  , nullTest td (u::Int32)
  , nullTest td (u::Int64)
  , nullTest td (u::Float)
  , nullTest td (u::Double)
  , nullTest td (u::Bool)
  , nullTest td (u::Char)
  , nullTest td (u::Word8)
  , nullTest td (u::String)
  , nullTest td (u::BS.ByteString)
  , nullTest td (u::T.Text)
  , nullTest td (u::Binary BS.ByteString)
  , nullTest td (u::Interval)
  , nullTest td (u::Day)
  , nullTest td (u::TimeOfDay)
  , nullTest td (u::LocalTime)
  , nullTest td (u::UTCTime)
  , nullTest td (u::ZonedTime)
  , nullTest td (u::Array1 Int32)
  , nullTest td (u::Array2 Double)
  , nullTest td (u::Composite Simple)
  , nullTest td (u::CompositeArray1 Simple)
  , nullTest td (u::CompositeArray2 Simple)
  ----------------------------------------
  , putGetTest td 100 (u::Int16) (==)
  , putGetTest td 100 (u::Int32) (==)
  , putGetTest td 100 (u::Int64) (==)
  , putGetTest td 10000 (u::Float) (==)
  , putGetTest td 10000 (u::Double) (==)
  , putGetTest td 100 (u::Bool) (==)
  , putGetTest td 100 (u::Char) (==)
  , putGetTest td 100 (u::Word8) (==)
  , putGetTest td 1000 (u::String0) (==)
  , putGetTest td 1000 (u::BS.ByteString) (==)
  , putGetTest td 1000 (u::T.Text) (==)
  , putGetTest td 1000 (u::Binary BS.ByteString) (==)
  , putGetTest td 50 (u::Interval) (==)
  , putGetTest td 1000000 (u::Day) (==)
  , putGetTest td 10000 (u::TimeOfDay) eqTOD
  , putGetTest td 500000 (u::LocalTime) eqLT
  , putGetTest td 500000 (u::UTCTime) eqUTCT
  , putGetTest td 500000 (u::ZonedTime) eqZT
  , putGetTest td 1000 (u::Array1 Int32) (==)
  , putGetTest td 1000 (u::Array2 Double) eqArray2
  , putGetTest td 100000 (u::Composite Simple) (==)
  , putGetTest td 1000 (u::CompositeArray1 Simple) (==)
  , putGetTest td 1000 (u::CompositeArray2 Simple) eqCompositeArray2
  , putGetTest td 100000 (u::Composite Nested) (==)
  , putGetTest td 1000 (u::CompositeArray1 Nested) (==)
  , putGetTest td 1000 (u::CompositeArray2 Nested) eqCompositeArray2
  ----------------------------------------
  , rowTest td (u::Single Int16)
  , rowTest td (u::(Int16, Int32))
  , rowTest td (u::(Int16, Int32, Int64))
  , rowTest td (u::(Int16, Int32, Int64, Float))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, Char))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, Char, Word8))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, Char))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, Char, Word8))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, Char, Word8, String0, BS.ByteString, T.Text, Binary BS.ByteString, Day, Array1 Int32))
  ]
  where
    u = undefined

----------------------------------------

createStructures :: ConnectionSource -> IO ()
createStructures cs = runDBT cs dts $ do
  liftBase . putStrLn $ "Creating structures..."
  runSQL_ "CREATE TABLE test1_ (a INTEGER)"
  runSQL_ "CREATE TYPE simple_ AS (a INTEGER, b DATE)"
  runSQL_ "CREATE TYPE nested_ AS (d DOUBLE PRECISION, s SIMPLE_)"

dropStructures :: ConnectionSource -> IO ()
dropStructures cs = runDBT cs dts $ do
  liftBase . putStrLn $ "Dropping structures..."
  runSQL_ "DROP TYPE nested_"
  runSQL_ "DROP TYPE simple_"
  runSQL_ "DROP TABLE test1_"

main :: IO ()
main = do
  args <- getArgs
  when (null args) $ do
    prog <- getProgName
    putStrLn $ "Usage:" <+> prog <+> "<connection info string> [test-framework args]"
    exitFailure

  let connSettings = ConnectionSettings {
          csConnInfo = BSC.pack $ head args
        , csClientEncoding = Just "latin1"
        , csComposites = []
        }
      connSource = defaultSource connSettings

  createStructures connSource
  connPool <- poolSource (connSettings { csComposites = ["simple_", "nested_"] }) 1 30 16
  gen <- getStdGen
  putStrLn $ "PRNG:" <+> show gen

  E.finally (defaultMainWithArgs (tests (gen, connPool)) $ tail args) $ do
    dropStructures connSource
