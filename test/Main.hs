{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Control.Applicative
import Control.Concurrent.Lifted
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.State
import Control.Monad.Trans.Control
import Data.Aeson
import Data.Char
import Data.Int
import Data.Maybe
import Data.Time
import Data.Typeable
import Data.Word
import Prelude
import System.Environment
import System.Exit
import System.Random
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test, assertEqual)
import Test.QuickCheck
import Test.QuickCheck.Gen
import TextShow
import qualified Data.ByteString as BS
import qualified Data.Text as T

import Data.Monoid.Utils
import Database.PostgreSQL.PQTypes
import Prelude.Instances ()
import Test.QuickCheck.Arbitrary.Instances
import Test.QuickCheck.Compat

type InnerTestEnv = StateT QCGen (DBT IO)

newtype TestEnv a = TestEnv { unTestEnv :: InnerTestEnv a }
  deriving (Applicative, Functor, Monad, MonadBase IO, MonadCatch, MonadDB, MonadMask, MonadThrow)

instance MonadBaseControl IO TestEnv where
#if MIN_VERSION_monad_control(1,0,0)
  type StM TestEnv a = StM InnerTestEnv a
  liftBaseWith f = TestEnv $ liftBaseWith $ \run ->
                         f $ run . unTestEnv
  restoreM = TestEnv . restoreM
#else
  newtype StM TestEnv a = StTestEnv { unStTestEnv :: StM InnerTestEnv a }
  liftBaseWith f = TestEnv $ liftBaseWith $ \run ->
                         f $ liftM StTestEnv . run . unTestEnv
  restoreM = TestEnv . restoreM . unStTestEnv
#endif

withQCGen :: (QCGen -> r) -> TestEnv r
withQCGen f = do
  gen <- TestEnv get
  TestEnv . modify $ snd . next
  return (f gen)

----------------------------------------

type TestData = (QCGen, ConnectionSourceM IO)

runTestEnv :: TestData -> TransactionSettings -> TestEnv a -> IO a
runTestEnv (env, cs) ts m = runDBT cs ts $ evalStateT (unTestEnv m) env

runTimes :: Monad m => Int -> m () -> m ()
runTimes !n m = case n of
  0 -> return ()
  _ -> m >> runTimes (n-1) m

----------------------------------------

newtype AsciiChar = AsciiChar { unAsciiChar :: Char }
  deriving (Eq, Show, Typeable, PQFormat)

instance ToSQL AsciiChar where
  type PQDest AsciiChar = PQDest Char
  toSQL = toSQL . unAsciiChar

instance FromSQL AsciiChar where
  type PQBase AsciiChar = PQBase Char
  fromSQL = fmap AsciiChar . fromSQL

instance Arbitrary AsciiChar where
  -- QuickCheck >= 2.10 changed Arbitrary Char instance to include proper
  -- Unicode CharS, but PostgreSQL only accepts ASCII ones.
  arbitrary = AsciiChar . chr <$> oneof [choose (0,127), choose (0,255)]
  shrink    = map AsciiChar . shrink . unAsciiChar

instance Arbitrary Interval where
  arbitrary = Interval
    <$> abs `fmap` arbitrary
    <*> choose (0, 11)
    <*> choose (0, 364)
    <*> choose (0, 23)
    <*> choose (0, 59)
    <*> choose (0, 59)
    <*> choose (0, 999999)

instance (Arbitrary a1, Arbitrary a2) => Arbitrary (a1 :*: a2) where
  arbitrary = (:*:) <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (Composite a) where
  arbitrary = Composite <$> arbitrary

instance Arbitrary json => Arbitrary (JSON json) where
  arbitrary = JSON <$> arbitrary

instance Arbitrary jsonb => Arbitrary (JSONB jsonb) where
  arbitrary = JSONB <$> arbitrary

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
  toComposite (a, b) = Simple a b
instance CompositeToSQL Simple where
  fromComposite (Simple a b) = (a, b)

instance Arbitrary Simple where
  arbitrary = Simple <$> arbitrary <*> arbitrary

data Nested = Nested (Maybe Double) (Maybe Simple)
  deriving (Eq, Ord, Show, Typeable)

type instance CompositeRow Nested = (Maybe Double, Maybe (Composite Simple))

instance PQFormat Nested where
  pqFormat _ = "%nested_"
instance CompositeFromSQL Nested where
  toComposite (a, b) = Nested a (unComposite <$> b)
instance CompositeToSQL Nested where
  fromComposite (Nested a b) = (a, Composite <$> b)

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

tsNoTrans :: TransactionSettings
tsNoTrans = def { tsAutoTransaction = False }

randomValue :: Arbitrary t => Int -> TestEnv t
randomValue n = withQCGen $ \gen -> unGen arbitrary gen n

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
autocommitTest td = testCase "Autocommit mode works" . runTestEnv td tsNoTrans $ do
  let sint = Identity (1::Int32)
  runQuery_ $ rawSQL "INSERT INTO test1_ (a) VALUES ($1)" sint
  withNewConnection $ do
    n <- runQuery $ rawSQL "SELECT a FROM test1_ WHERE a = $1" sint
    assertEqual "Other connection sees autocommited data" n 1 (==)
  runQuery_ $ rawSQL "DELETE FROM test1_ WHERE a = $1" sint

readOnlyTest :: TestData -> Test
readOnlyTest td = testCase "Read only transaction mode works" . runTestEnv td def{tsPermissions = ReadOnly} $ do
  let sint = Identity (2::Int32)
  eres <- try . runQuery_ $ rawSQL "INSERT INTO test1_ (a) VALUES ($1)" sint
  case eres :: Either DBException () of
    Left _ -> return ()
    Right _ -> liftBase . assertFailure $ "DBException wasn't thrown"
  rollback
  n <- runQuery $ rawSQL "SELECT a FROM test1_ WHERE a = $1" sint
  assertEqual "SELECT works in read only mode" n 0 (==)

savepointTest :: TestData -> Test
savepointTest td = testCase "Savepoint support works" . runTestEnv td def $ do
  let int1 = 3 :: Int32
      int2 = 4 :: Int32

  -- action executed within withSavepoint throws
  runQuery_ $ rawSQL "INSERT INTO test1_ (a) VALUES ($1)" (Identity int1)
  _ :: Either DBException () <- try . withSavepoint (Savepoint "test") $ do
    runQuery_ $ rawSQL "INSERT INTO test1_ (a) VALUES ($1)" (Identity int2)
    runSQL_ "SELECT * FROM table_that_is_not_there"
  runQuery_ $ rawSQL "SELECT a FROM test1_ WHERE a IN ($1, $2)" (int1, int2)
  res1 <- fetchMany runIdentity
  assertEqual "Part of transaction was rolled back" res1 [int1] (==)

  rollback

  -- action executed within withSavepoint doesn't throw
  runQuery_ $ rawSQL "INSERT INTO test1_ (a) VALUES ($1)" (Identity int1)
  withSavepoint (Savepoint "test") $ do
    runQuery_ $ rawSQL "INSERT INTO test1_ (a) VALUES ($1)" (Identity int2)
  runQuery_ $ rawSQL "SELECT a FROM test1_ WHERE a IN ($1, $2) ORDER BY a" (int1, int2)
  res2 <- fetchMany runIdentity
  assertEqual "Result of all queries is visible" res2 [int1, int2] (==)

notifyTest :: TestData -> Test
notifyTest td = testCase "Notifications work" . runTestEnv td tsNoTrans $ do
  listen chan
  forkNewConn $ notify chan payload
  mnt1 <- getNotification 100000
  liftBase $ assertBool "Notification received" (isJust mnt1)
  let Just nt1 = mnt1
  assertEqual "Channels are equal" chan (ntChannel nt1) (==)
  assertEqual "Payloads are equal" payload (ntPayload nt1) (==)

  unlisten chan
  forkNewConn $ notify chan payload
  mnt2 <- getNotification 100000
  assertEqual "No notification received after unlisten" Nothing mnt2 (==)

  listen chan
  unlistenAll
  forkNewConn $ notify chan payload
  mnt3 <- getNotification 100000
  assertEqual "No notification received after unlistenAll" Nothing mnt3 (==)
  where
    chan = "test_channel"
    payload = "test_payload"
    forkNewConn = void . fork . withNewConnection

transactionTest :: TestData -> IsolationLevel -> Test
transactionTest td lvl = testCase ("Auto transaction works by default with isolation level" <+> show lvl) . runTestEnv td def{tsIsolationLevel = lvl} $ do
  let sint = Identity (5::Int32)
  runQuery_ $ rawSQL "INSERT INTO test1_ (a) VALUES ($1)" sint
  withNewConnection $ do
    n <- runQuery $ rawSQL "SELECT a FROM test1_ WHERE a = $1" sint
    assertEqual "Other connection doesn't see uncommited data" n 0 (==)
  rollback

nullTest :: forall t. (Show t, ToSQL t, FromSQL t, Typeable t)
         => TestData -> t -> Test
nullTest td t = testCase ("Attempt to get non-NULL value of type" <+> show (typeOf t) <+> "fails if NULL is provided") . runTestEnv td def $ do
  runSQL_ $ "SELECT" <?> (Nothing::Maybe t)
  eres  <- try $ fetchOne runIdentity
  case eres :: Either DBException t of
    Left _ -> return ()
    Right _ -> liftBase . assertFailure $ "DBException wasn't thrown"

putGetTest :: forall t. (Arbitrary t, Show t, ToSQL t, FromSQL t, Typeable t)
           => TestData -> Int -> t -> (t -> t -> Bool) -> Test
putGetTest td n t eq = testCase ("Putting value of type" <+> show (typeOf t) <+> "through database doesn't change its value") . runTestEnv td def . runTimes 1000 $ do
  v :: t <- randomValue n
  --liftBase . putStrLn . show $ v
  runSQL_ $ "SELECT" <?> v
  v' <- fetchOne runIdentity
  assertEqual "Value doesn't change after getting through database" v v' eq

xmlTest :: TestData -> Test
xmlTest td  = testCase "Put and get XML value works" . runTestEnv td def $ do
  runSQL_ $ "SET CLIENT_ENCODING TO 'UTF8'"
  let v = XML "some<tag>stringå</tag>"
  runSQL_ $ "SELECT XML 'some<tag>stringå</tag>'"
  v' <- fetchOne runIdentity
  assertEqual "XML value correct" v v' (==)
  runSQL_ $ "SELECT" <?> v
  v'' <- fetchOne runIdentity
  assertEqual "XML value correct" v v'' (==)
  runSQL_ $ "SET CLIENT_ENCODING TO 'latin-1'"

rowTest :: forall row. (Arbitrary row, Eq row, Show row, ToRow row, FromRow row)
        => TestData -> row -> Test
rowTest td r = testCase ("Putting row of length" <+> show (pqVariables r) <+> "through database works") . runTestEnv td def . runTimes 100 $ do
  row :: row <- randomValue 100
  let fmt = mintercalate ", " $ map (T.append "$" . showt) [1..pqVariables r]
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
  , xmlTest td
  , readOnlyTest td
  , savepointTest td
  , notifyTest td
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
  , nullTest td (u::AsciiChar)
  , nullTest td (u::Word8)
  , nullTest td (u::String)
  , nullTest td (u::BS.ByteString)
  , nullTest td (u::T.Text)
  , nullTest td (u::JSON Value)
  , nullTest td (u::JSONB Value)
  , nullTest td (u::XML)
  , nullTest td (u::Interval)
  , nullTest td (u::Day)
  , nullTest td (u::TimeOfDay)
  , nullTest td (u::LocalTime)
  , nullTest td (u::UTCTime)
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
  , putGetTest td 100 (u::AsciiChar) (==)
  , putGetTest td 100 (u::Word8) (==)
  , putGetTest td 1000 (u::String0) (==)
  , putGetTest td 1000 (u::BS.ByteString) (==)
  , putGetTest td 1000 (u::T.Text) (==)
  , putGetTest td 50 (u::JSON Value) (==)
  , putGetTest td 50 (u::JSONB Value) (==)
  , putGetTest td 20 (u::Array1 (JSON Value)) (==)
  , putGetTest td 20 (u::Array1 (JSONB Value)) (==)
  , putGetTest td 50 (u::Interval) (==)
  , putGetTest td 1000000 (u::Day) (==)
  , putGetTest td 10000 (u::TimeOfDay) eqTOD
  , putGetTest td 500000 (u::LocalTime) eqLT
  , putGetTest td 500000 (u::UTCTime) eqUTCT
  , putGetTest td 1000 (u::Array1 Int32) (==)
  , putGetTest td 1000 (u::Array2 Double) eqArray2
  , putGetTest td 100000 (u::Composite Simple) (==)
  , putGetTest td 1000 (u::CompositeArray1 Simple) (==)
  , putGetTest td 1000 (u::CompositeArray2 Simple) eqCompositeArray2
  , putGetTest td 100000 (u::Composite Nested) (==)
  , putGetTest td 1000 (u::CompositeArray1 Nested) (==)
  , putGetTest td 1000 (u::CompositeArray2 Nested) eqCompositeArray2
  ----------------------------------------
  , rowTest td (u::Identity Int16)
  , rowTest td (u::Identity T.Text :*: (Double, Int16))
  , rowTest td (u::(T.Text, Double) :*: Identity Int16)
  , rowTest td (u::(Int16, T.Text, Int64, Double) :*: Identity Bool :*: (String0, AsciiChar))
  , rowTest td (u::(Int16, Int32))
  , rowTest td (u::(Int16, Int32, Int64))
  , rowTest td (u::(Int16, Int32, Int64, Float))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, AsciiChar))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, AsciiChar))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32))
  ]
  where
    u = undefined

----------------------------------------

createStructures :: ConnectionSourceM IO -> IO ()
createStructures cs = runDBT cs def $ do
  liftBase . putStrLn $ "Creating structures..."
  runSQL_ "CREATE TABLE test1_ (a INTEGER)"
  runSQL_ "CREATE TYPE simple_ AS (a INTEGER, b DATE)"
  runSQL_ "CREATE TYPE nested_ AS (d DOUBLE PRECISION, s SIMPLE_)"

dropStructures :: ConnectionSourceM IO -> IO ()
dropStructures cs = runDBT cs def $ do
  liftBase . putStrLn $ "Dropping structures..."
  runSQL_ "DROP TYPE nested_"
  runSQL_ "DROP TYPE simple_"
  runSQL_ "DROP TABLE test1_"

getConnString :: IO (T.Text, [String])
getConnString = do
  args <- getArgs
  if (null args) then
    do isTravis <- maybe False ((==) "true") <$> lookupEnv "TRAVIS"
       if isTravis
         then return ("postgresql://postgres@localhost/travis_ci_test", [])
         else do printUsage
                 exitFailure
    else return $ (T.pack . head $ args, tail args)
  where
    printUsage = do
      prog <- getProgName
      putStrLn $ "Usage:" <+> prog
        <+> "<connection info string> [test-framework args]"

main :: IO ()
main = do
  (connString, args) <- getConnString
  let connSettings = def {
          csConnInfo       = connString
        , csClientEncoding = Just "latin1"
        }
      ConnectionSource connSource = simpleSource connSettings

  createStructures connSource
  ConnectionSource connPool <-
    poolSource (connSettings { csComposites = ["simple_", "nested_"] }) 1 30 16
  gen <- newQCGen
  putStrLn $ "PRNG:" <+> show gen

  finally (defaultMainWithArgs (tests (gen, connPool)) $ args) $ do
    dropStructures connSource
