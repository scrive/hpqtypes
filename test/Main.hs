{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Control.Concurrent.Lifted
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.State
import Control.Monad.Trans.Control
import Data.Aeson hiding ((<?>))
import Data.Char
import Data.Int
import Data.Maybe
import Data.Time
import Data.Typeable
import Data.Word
import System.Environment
import System.Exit
import System.Random
import System.Timeout.Lifted
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test, assertEqual)
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import TextShow
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.UUID.Types as U

import Data.Monoid.Utils
import Database.PostgreSQL.PQTypes
import Prelude.Instances ()
import Test.Aeson.Compat (Value0)
import Test.QuickCheck.Arbitrary.Instances

type InnerTestEnv = StateT QCGen (DBT IO)

newtype TestEnv a = TestEnv { unTestEnv :: InnerTestEnv a }
  deriving ( Applicative, Functor, Monad, MonadFail
           , MonadBase IO, MonadCatch, MonadDB, MonadMask, MonadThrow)

instance MonadBaseControl IO TestEnv where
  type StM TestEnv a = StM InnerTestEnv a
  liftBaseWith f = TestEnv $ liftBaseWith $ \run ->
                         f $ run . unTestEnv
  restoreM = TestEnv . restoreM

withQCGen :: (QCGen -> r) -> TestEnv r
withQCGen f = do
  gen <- TestEnv $ state split
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
  deriving (Eq, Show)

instance PQFormat AsciiChar where
  pqFormat = pqFormat @Char

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
  deriving (Eq, Ord, Show)

type instance CompositeRow Simple = (Maybe Int32, Maybe Day)

instance PQFormat Simple where
  pqFormat = "%simple_"
instance CompositeFromSQL Simple where
  toComposite (a, b) = Simple a b
instance CompositeToSQL Simple where
  fromComposite (Simple a b) = (a, b)

instance Arbitrary Simple where
  arbitrary = Simple <$> arbitrary <*> arbitrary

data Nested = Nested (Maybe Double) (Maybe Simple)
  deriving (Eq, Ord, Show)

type instance CompositeRow Nested = (Maybe Double, Maybe (Composite Simple))

instance PQFormat Nested where
  pqFormat = "%nested_"
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
tsNoTrans = defaultTransactionSettings { tsAutoTransaction = False }

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

assertEqualEq :: (Eq a, Show a, MonadBase IO m) => String -> a -> a -> m ()
assertEqualEq preface expected actual = assertEqual preface expected actual (==)

----------------------------------------

sqlGenInts :: Int32 -> SQL
sqlGenInts n = smconcat
  [ "WITH RECURSIVE ints(n) AS"
  , "( VALUES (1) UNION ALL SELECT n+1 FROM ints WHERE n <" <?> n
  , ") SELECT n FROM ints"
  ]

cursorTest :: TestData -> Test
cursorTest td = testGroup "Cursors"
  [ basicCursorWorks
  , scrollableCursorWorks
  , withHoldCursorWorks
  , doubleCloseWorks
  ]
  where
    basicCursorWorks = testCase "Basic cursor works" $ do
      runTestEnv td defaultTransactionSettings $ do
        withCursor "ints" NoScroll NoHold (sqlGenInts 5) $ \cursor -> do
          xs <- (`fix` []) $ \loop acc -> cursorFetch cursor CD_Next >>= \case
            0 -> return $ reverse acc
            1 -> do
              (n::Int32) <- fetchOne runIdentity
              loop $ n : acc
            n -> error $ "Unexpected number of rows: " ++ show n
          assertEqualEq "Data fetched correctly" [1..5] xs

    scrollableCursorWorks = testCase "Cursor declared as SCROLL works" $ do
      runTestEnv td defaultTransactionSettings $ do
        withCursor "ints" Scroll NoHold (sqlGenInts 10) $ \cursor -> do
          checkMove cursor CD_Next         1
          checkMove cursor CD_Prior        0
          checkMove cursor CD_First        1
          checkMove cursor CD_Last         1
          checkMove cursor CD_Backward_All 9
          checkMove cursor CD_Forward_All  10
          checkMove cursor (CD_Absolute 0) 0
          checkMove cursor (CD_Relative 0) 0
          checkMove cursor (CD_Forward 5)  5
          checkMove cursor (CD_Backward 5) 4

          cursorFetch_ cursor CD_Forward_All
          xs1::[Int32] <- fetchMany runIdentity
          assertEqualEq "xs1 is correct" [1..10] xs1
          cursorFetch_ cursor CD_Backward_All
          xs2::[Int32] <- fetchMany runIdentity
          assertEqualEq "xs2 is correct" (reverse [1..10]) xs2
      where
        checkMove cursor cd expected = do
          moved <- cursorMove cursor cd
          assertEqualEq ("Moving cursor with" <+> show cd
                         <+> "would fetch a correct amount of rows")
            expected moved

    withHoldCursorWorks = testCase "Cursor declared as WITH HOLD works" $ do
      runTestEnv td tsNoTrans $ do
        withCursor "ints" NoScroll Hold (sqlGenInts 10) $ \cursor -> do
          cursorFetch_ cursor CD_Forward_All
          sum_::Int32 <- sum . fmap runIdentity <$> queryResult
          assertEqualEq "sum_ is correct" 55 sum_

    doubleCloseWorks = testCase "Double CLOSE works on a cursor" $ do
      runTestEnv td defaultTransactionSettings $ do
        withCursorSQL "ints" NoScroll NoHold "SELECT 1" $ \_cursor -> do
          -- Commiting a transaction closes the cursor
          commit

queryInterruptionTest :: TestData -> Test
queryInterruptionTest td = testCase "Queries are interruptible" $ do
  let sleep = "SELECT pg_sleep(2)"
      ints  = sqlGenInts 5000000
  runTestEnv td tsNoTrans $ do
    testQuery id sleep
    testQuery id ints
  runTestEnv td defaultTransactionSettings $ do
    testQuery (withSavepoint "ints")  ints
    testQuery (withSavepoint "sleep") sleep
   where
    testQuery m sql = timeout 500000 (m $ runSQL_ sql) >>= \case
      Just _  -> liftBase $ do
        assertFailure $ "Query" <+> show sql <+> "wasn't interrupted in time"
      Nothing -> return ()

autocommitTest :: TestData -> Test
autocommitTest td = testCase "Autocommit mode works" .
                    runTestEnv td tsNoTrans $ do
  let sint = Identity (1::Int32)
  runQuery_ $ rawSQL "INSERT INTO test1_ (a) VALUES ($1)" sint
  withNewConnection $ do
    n <- runQuery $ rawSQL "SELECT a FROM test1_ WHERE a = $1" sint
    assertEqualEq "Other connection sees autocommited data" 1 n
  runQuery_ $ rawSQL "DELETE FROM test1_ WHERE a = $1" sint

readOnlyTest :: TestData -> Test
readOnlyTest td = testCase "Read only transaction mode works" .
                  runTestEnv td
                  defaultTransactionSettings {tsPermissions = ReadOnly} $ do
  let sint = Identity (2::Int32)
  eres <- try . runQuery_ $ rawSQL "INSERT INTO test1_ (a) VALUES ($1)" sint
  case eres :: Either DBException () of
    Left  _ -> return ()
    Right _ -> liftBase . assertFailure $ "DBException wasn't thrown"
  rollback
  n <- runQuery $ rawSQL "SELECT a FROM test1_ WHERE a = $1" sint
  assertEqualEq "SELECT works in read only mode" 0 n

savepointTest :: TestData -> Test
savepointTest td = testCase "Savepoint support works" .
                   runTestEnv td defaultTransactionSettings $ do
  let int1 = 3 :: Int32
      int2 = 4 :: Int32

  -- action executed within withSavepoint throws
  runQuery_ $ rawSQL "INSERT INTO test1_ (a) VALUES ($1)" (Identity int1)
  _ :: Either DBException () <- try . withSavepoint (Savepoint "test") $ do
    runQuery_ $ rawSQL "INSERT INTO test1_ (a) VALUES ($1)" (Identity int2)
    runSQL_ "SELECT * FROM table_that_is_not_there"
  runQuery_ $ rawSQL "SELECT a FROM test1_ WHERE a IN ($1, $2)" (int1, int2)
  res1 <- fetchMany runIdentity
  assertEqualEq "Part of transaction was rolled back" [int1] res1

  rollback

  -- action executed within withSavepoint doesn't throw
  runQuery_ $ rawSQL "INSERT INTO test1_ (a) VALUES ($1)" (Identity int1)
  withSavepoint (Savepoint "test") $ do
    runQuery_ $ rawSQL "INSERT INTO test1_ (a) VALUES ($1)" (Identity int2)
  runQuery_ $ rawSQL
    "SELECT a FROM test1_ WHERE a IN ($1, $2) ORDER BY a" (int1, int2)
  res2 <- fetchMany runIdentity
  assertEqualEq "Result of all queries is visible" [int1, int2] res2

notifyTest :: TestData -> Test
notifyTest td = testCase "Notifications work" . runTestEnv td tsNoTrans $ do
  listen chan
  forkNewConn $ notify chan payload
  mnt1 <- getNotification 100000
  liftBase $ assertBool "Notification received" (isJust mnt1)
  Just nt1 <- pure mnt1
  assertEqualEq "Channels are equal" chan (ntChannel nt1)
  assertEqualEq "Payloads are equal" payload (ntPayload nt1)

  unlisten chan
  forkNewConn $ notify chan payload
  mnt2 <- getNotification 100000
  assertEqualEq "No notification received after unlisten" Nothing mnt2

  listen chan
  unlistenAll
  forkNewConn $ notify chan payload
  mnt3 <- getNotification 100000
  assertEqualEq "No notification received after unlistenAll" Nothing mnt3
  where
    chan = "test_channel"
    payload = "test_payload"
    forkNewConn = void . fork . withNewConnection

transactionTest :: TestData -> IsolationLevel -> Test
transactionTest td lvl =
  testCase
  ("Auto transaction works by default with isolation level"
    <+> show lvl) .
  runTestEnv td
  defaultTransactionSettings {tsIsolationLevel = lvl} $ do
  let sint = Identity (5::Int32)
  runQuery_ $ rawSQL "INSERT INTO test1_ (a) VALUES ($1)" sint
  withNewConnection $ do
    n <- runQuery $ rawSQL "SELECT a FROM test1_ WHERE a = $1" sint
    assertEqualEq "Other connection doesn't see uncommited data" 0 n
  rollback

nullTest :: forall t. (Show t, ToSQL t, FromSQL t, Typeable t)
         => TestData -> t -> Test
nullTest td t = testCase ("Attempt to get non-NULL value of type"
                          <+> show (typeOf t) <+> "fails if NULL is provided") .
                runTestEnv td defaultTransactionSettings $ do
  runSQL_ $ "SELECT" <?> (Nothing::Maybe t)
  eres  <- try $ fetchOne runIdentity
  case eres :: Either DBException t of
    Left _ -> return ()
    Right _ -> liftBase . assertFailure $ "DBException wasn't thrown"

putGetTest :: forall t. (Arbitrary t, Show t, ToSQL t, FromSQL t, Typeable t)
           => TestData -> Int -> t -> (t -> t -> Bool) -> Test
putGetTest td n t eq = testCase
                       ("Putting value of type"
                         <+> show (typeOf t)
                         <+> "through database doesn't change its value") .
                       runTestEnv td defaultTransactionSettings .
                       runTimes 1000 $ do
  v :: t <- randomValue n
  --liftBase . putStrLn . show $ v
  runSQL_ $ "SELECT" <?> v
  v' <- fetchOne runIdentity
  assertEqual "Value doesn't change after getting through database" v v' eq

uuidTest :: TestData -> Test
uuidTest td = testCase "UUID encoding / decoding test" $ do
  let uuidStr = "550e8400-e29b-41d4-a716-446655440000"
  Just uuid <- pure $ U.fromText uuidStr
  runTestEnv td defaultTransactionSettings $ do
    runSQL_ $ mkSQL $ "SELECT '" `mappend` uuidStr `mappend` "' :: uuid"
    uuid2 <- fetchOne runIdentity
    assertEqual "UUID is decoded correctly" uuid uuid2 (==)

    runQuery_ $ rawSQL " SELECT $1 :: text" (Identity uuid)
    uuidStr2 <- fetchOne runIdentity
    assertEqual "UUID is encoded correctly" uuidStr uuidStr2 (==)

xmlTest :: TestData -> Test
xmlTest td  = testCase "Put and get XML value works" .
              runTestEnv td defaultTransactionSettings $ do
  runSQL_ $ "SET CLIENT_ENCODING TO 'UTF8'"
  let v = XML "some<tag>stringå</tag>"
  runSQL_ $ "SELECT XML 'some<tag>stringå</tag>'"
  v' <- fetchOne runIdentity
  assertEqualEq "XML value correct" v v'
  runSQL_ $ "SELECT" <?> v
  v'' <- fetchOne runIdentity
  assertEqualEq "XML value correct" v v''
  runSQL_ $ "SET CLIENT_ENCODING TO 'latin-1'"

rowTest :: forall row. (Arbitrary row, Eq row, Show row, ToRow row, FromRow row)
        => TestData -> row -> Test
rowTest td _r = testCase ("Putting row of length"
                          <+> show (pqVariables @row)
                          <+> "through database works") .
                runTestEnv td defaultTransactionSettings . runTimes 100 $ do
  row :: row <- randomValue 100
  let fmt = mintercalate ", " $ map (T.append "$" . showt) [1..pqVariables @row]
  runQuery_ $ rawSQL ("SELECT" <+> fmt) row
  row' <- fetchOne id
  assertEqualEq "Row doesn't change after getting through database" row row'

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
  , queryInterruptionTest td
  , cursorTest td
  , uuidTest td
  ------------------------------------
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
  , nullTest td (u::U.UUID)
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
  , putGetTest td 1000 (u::U.UUID) (==)
  , putGetTest td 50 (u::JSON Value0) (==)
  , putGetTest td 50 (u::JSONB Value0) (==)
  , putGetTest td 20 (u::Array1 (JSON Value0)) (==)
  , putGetTest td 20 (u::Array1 (JSONB Value0)) (==)
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
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, U.UUID))
  , rowTest td (u::(Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, Day, Array1 Int32, Composite Simple, CompositeArray1 Simple, Composite Nested, CompositeArray1 Nested, Int16, Int32, Int64, Float, Double, Bool, AsciiChar, Word8, String0, BS.ByteString, T.Text, BS.ByteString, U.UUID, Day))
  ]
  where
    u = undefined

----------------------------------------

createStructures :: ConnectionSourceM IO -> IO ()
createStructures cs = runDBT cs defaultTransactionSettings $ do
  liftBase . putStrLn $ "Creating structures..."
  runSQL_ "CREATE TABLE test1_ (a INTEGER)"
  runSQL_ "CREATE TYPE simple_ AS (a INTEGER, b DATE)"
  runSQL_ "CREATE TYPE nested_ AS (d DOUBLE PRECISION, s SIMPLE_)"

dropStructures :: ConnectionSourceM IO -> IO ()
dropStructures cs = runDBT cs defaultTransactionSettings $ do
  liftBase . putStrLn $ "Dropping structures..."
  runSQL_ "DROP TYPE nested_"
  runSQL_ "DROP TYPE simple_"
  runSQL_ "DROP TABLE test1_"

getConnString :: IO (T.Text, [String])
getConnString = getArgs >>= \case
  connString : args -> return (T.pack connString, args)
  [] -> lookupEnv "GITHUB_ACTIONS" >>= \case
    Just "true" -> return ("host=postgres user=postgres password=postgres", [])
    _           -> printUsage >> exitFailure
  where
    printUsage = do
      prog <- getProgName
      putStrLn $ "Usage:" <+> prog
        <+> "<connection info string> [test-framework args]"

main :: IO ()
main = do
  (connString, args) <- getConnString
  let connSettings = defaultConnectionSettings {
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
