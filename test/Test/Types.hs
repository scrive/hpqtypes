{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Tests of encoding and decoding of the supported types: generic NULL and
-- roundtrip tests over the full set of instances, plus type-specific ones.
module Test.Types
  ( typesTests
  ) where

import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Data.Aeson (Value)
import Data.ByteString qualified as BS
import Data.IP (IPRange)
import Data.Int
import Data.Scientific
import Data.Text qualified as T
import Data.Time
import Data.Typeable
import Data.UUID.Types qualified as U
import Data.Vector qualified as V
import Data.Word
import Test.QuickCheck
import Test.Tasty
import TextShow

import Data.Monoid.Utils
import Database.PostgreSQL.PQTypes
import Test.Env
import Test.QuickCheck.Arbitrary.Instances

typesTests :: TestData -> [TestTree]
typesTests td =
  [ uuidTest td
  , integerTest td
  , fractionalNumericTest td
  , xmlTest td
  , intervalComparisonTest td
  , intWordEncodingTest td
  , rangeTest td
  , nullTest @Int16 td
  , nullTest @Int32 td
  , nullTest @Int64 td
  , nullTest @Float td
  , nullTest @Double td
  , nullTest @Bool td
  , nullTest @AsciiChar td
  , nullTest @Word8 td
  , nullTest @Word16 td
  , nullTest @Word32 td
  , nullTest @Word64 td
  , nullTest @Integer td
  , nullTest @Scientific td
  , nullTest @String td
  , nullTest @BS.ByteString td
  , nullTest @T.Text td
  , nullTest @U.UUID td
  , nullTest @(JSON Value) td
  , nullTest @(JSONB Value) td
  , nullTest @XML td
  , nullTest @Interval td
  , nullTest @Day td
  , nullTest @TimeOfDay td
  , nullTest @LocalTime td
  , nullTest @UTCTime td
  , nullTest @[Int32] td
  , nullTest @[[Double]] td
  , nullTest @(V.Vector Int32) td
  , nullTest @(V.Vector (V.Vector Double)) td
  , nullTest @IPRange td
  , nullTest @(Range Int32) td
  , putGetTest @Int16 td 100
  , putGetTest @Int32 td 100
  , putGetTest @Int64 td 100
  , putGetTest @Float td 10000
  , putGetTest @Double td 10000
  , putGetTest @Bool td 100
  , putGetTest @AsciiChar td 100
  , putGetTest @Word8 td 100
  , putGetTest @Word16 td 100
  , putGetTest @Word32 td 100
  , putGetTest @Word64 td 100
  , putGetTest @Integer td 1000000000000
  , putGetTest @Scientific td 1000000000000
  , putGetTest @String0 td 1000
  , putGetTest @BS.ByteString td 1000
  , putGetTest @T.Text td 1000
  , putGetTest @U.UUID td 1000
  , putGetTest @(JSON Value0) td 50
  , putGetTest @(JSONB Value0) td 50
  , putGetTest @Interval td 50
  , putGetTest @Day td 1000000
  , putGetTest @TimeOfDay td 10000
  , putGetTest @LocalTime td 500000
  , putGetTest @UTCTime td 500000
  , putGetTest @[Int32] td 1000
  , putGetTest @[Maybe Int32] td 1000
  , putGetTest @[String0] td 100
  , putGetTest @(Matrix Double) td 1000
  , putGetTest @[JSON Value0] td 20
  , putGetTest @[JSONB Value0] td 20
  , putGetTest @(V.Vector Int32) td 1000
  , putGetTest @(V.Vector (Maybe Int32)) td 1000
  , putGetTest @(VMatrix Double) td 1000
  , putGetTest @IPRange td 100
  , putGetTest @(Range Int32) td 100
  , putGetTest @(Range Int64) td 100
  , putGetTest @(Range Scientific) td 1000000000000
  , putGetTest @(Range Day) td 1000000
  , putGetTest @(Range LocalTime) td 500000
  , putGetTest @(Range UTCTime) td 500000
  ]

----------------------------------------

nullTest
  :: forall a
   . (Show a, ToSQL a, FromSQL a, Typeable a)
  => TestData
  -> TestTree
nullTest td = testCase
  ( "Attempt to get non-NULL value of type"
      <+> show (typeRep $ Proxy @a)
      <+> "fails if NULL is provided"
  )
  . runTestEnv td defaultTransactionSettings
  $ do
    runSQL_ $ "SELECT" <?> Nothing @a
    eres <- try $ fetchOne (fromSQL @a)
    case eres :: Either DBException a of
      Left _ -> pure ()
      Right _ -> liftBase . assertFailure $ "DBException wasn't thrown"

putGetTest
  :: forall a
   . (Arbitrary a, Eq a, Show a, ToSQL a, FromSQL a, Typeable a)
  => TestData
  -> Int
  -> TestTree
putGetTest td n = testCase
  ( "Putting value of type"
      <+> show (typeRep $ Proxy @a)
      <+> "through database doesn't change its value"
  )
  . runTestEnv td defaultTransactionSettings
  . replicateM_ 1000
  $ do
    v <- randomValue @a n
    -- liftBase . putStrLn . show $ v
    runSQL_ $ "SELECT" <?> v
    v' <- fetchOne fromSQL
    assertEqual "Value doesn't change after getting through database" v v'

----------------------------------------

uuidTest :: TestData -> TestTree
uuidTest td = testCase "UUID encoding / decoding test" $ do
  let uuidStr = "550e8400-e29b-41d4-a716-446655440000"
  Just uuid <- pure $ U.fromText uuidStr
  runTestEnv td defaultTransactionSettings $ do
    runSQL_ . mkSQL $ ("SELECT '" `mappend` uuidStr `mappend` "' :: uuid")
    uuid2 <- fetchOne fromSQL
    assertEqual "UUID is decoded correctly" uuid uuid2

    runQuery_ $ rawSQL " SELECT $1 :: text" (Identity uuid)
    uuidStr2 <- fetchOne fromSQL
    assertEqual "UUID is encoded correctly" uuidStr uuidStr2

integerTest :: TestData -> TestTree
integerTest td = testCase "Integer decoding from numeric works"
  . runTestEnv td defaultTransactionSettings
  . forM_ values
  $ \n -> do
    -- The server strips trailing zero base-10000 digit groups from the wire
    -- representation of numeric, so values that are multiples of 10000 arrive
    -- with fewer digits than their weight indicates.
    runSQL_ . mkSQL $ "SELECT " <> showt n <> " :: numeric"
    n' <- fetchOne fromSQL
    assertEqual ("Integer" <+> show n <+> "is decoded correctly") n n'

    runQuery_ $ rawSQL "SELECT $1" (Identity n)
    n'' <- fetchOne fromSQL
    assertEqual ("Integer" <+> show n <+> "roundtrips correctly") n n''
  where
    values :: [Integer]
    values =
      [ 0
      , 1
      , -1
      , 9999
      , 10000
      , -10000
      , 10001
      , 99990000
      , 100000000
      , 1000000000000
      , -1000000000000
      , 123400005678
      , 10 ^ (100 :: Int)
      , negate $ 10 ^ (100 :: Int)
      , 10 ^ (100 :: Int) + 1
      ]

fractionalNumericTest :: TestData -> TestTree
fractionalNumericTest td = testCase
  "Integer decoding from fractional numeric fails"
  . runTestEnv td defaultTransactionSettings
  $ do
    runSQL_ "SELECT 3.14 :: numeric"
    expectError @HPQTypesError "fractional numeric" (\_ -> pure ()) $
      fetchOne (fromSQL @Integer)

xmlTest :: TestData -> TestTree
xmlTest td = testCase "Put and get XML value works"
  . runTestEnv td defaultTransactionSettings
  $ do
    runSQL_ "SET CLIENT_ENCODING TO 'UTF8'"
    let v = XML "some<tag>stringå</tag>"
    runSQL_ "SELECT XML 'some<tag>stringå</tag>'"
    v' <- fetchOne fromSQL
    assertEqual "XML value correct" v v'
    runSQL_ $ "SELECT" <?> v
    v'' <- fetchOne fromSQL
    assertEqual "XML value correct" v v''
    runSQL_ "SET CLIENT_ENCODING TO 'latin-1'"

intervalComparisonTest :: TestData -> TestTree
intervalComparisonTest td = testCase
  "Eq and Ord of Interval match the comparison operators of the server"
  . runTestEnv td defaultTransactionSettings
  $ do
    -- The comparison estimate converts months at 30 days and days at 24
    -- hours.
    assertEqual "1 month equals 30 days" EQ $ compare (imonths 1) (idays 30)
    assertEqual "1 day equals 24 hours" EQ $ compare (idays 1) (ihours 24)
    assertEqual "Mixed signs cancel out" mempty $ imonths 1 <> idays (-30)
    replicateM_ 100 $ do
      a <- randomValue @Interval 100
      b <- randomValue @Interval 100
      runQuery_ $ rawSQL "SELECT $1 < $2, $1 = $2" (a, b)
      (lt, eq) <- fetchOne ((,) <$> fromSQL @Bool <*> fromSQL @Bool)
      let expected
            | eq = EQ
            | lt = LT
            | otherwise = GT
      assertEqual "Ordering matches the server" expected $ compare a b

-- | 'Int' and 'Word' have no 'FromSQL' instances (their size is
-- architecture-dependent), so their encoding is checked by fetching the
-- values back as 'Int64' and 'Word64'.
intWordEncodingTest :: TestData -> TestTree
intWordEncodingTest td = testCase
  "Int and Word parameters are encoded correctly"
  . runTestEnv td defaultTransactionSettings
  . replicateM_ 100
  $ do
    int <- randomValue @Int 1000000
    word <- randomValue @Word 1000000
    runQuery_ $ rawSQL "SELECT $1, $2" (int, word)
    result <- fetchOne ((,) <$> fromSQL @Int64 <*> fromSQL @Word64)
    assertEqual "Values are correct" (fromIntegral int, fromIntegral word) result

rangeTest :: TestData -> TestTree
rangeTest td = testCase "Range decoding works" $ do
  runTestEnv td defaultTransactionSettings $ do
    -- The first range is canonicalized by the server to [2,6).
    runSQL_ $
      mconcat
        [ "SELECT int4range(1, 5, '(]')"
        , ", 'empty'::int4range"
        , ", int4range(NULL, 5)"
        , ", numrange(1.5, 2.5, '(]')"
        , ", ARRAY['empty'::int8range, int8range(1, NULL)]"
        ]
    result <-
      fetchOne $
        (,,,,)
          <$> fromSQL @(Range Int32)
          <*> fromSQL @(Range Int32)
          <*> fromSQL @(Range Int32)
          <*> fromSQL @(Range Scientific)
          <*> fromSQL @[Range Int64]
    assertEqual
      "Result is correct"
      ( Range (Incl 2) (Excl 6)
      , Empty
      , Range Inf (Excl 5)
      , Range (Excl 1.5) (Incl 2.5)
      , [Empty, Range (Incl 1) Inf]
      )
      result
