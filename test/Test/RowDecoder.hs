-- | Tests of the row decoder: decoding of composite values (including
-- generic deriving) and detection of decoder errors.
module Test.RowDecoder
  ( rowDecoderTests
  , Simple (..)
  ) where

import Data.Int
import Data.Text qualified as T
import Data.Time
import GHC.Generics (Generic)
import Test.QuickCheck
import Test.Tasty

import Database.PostgreSQL.PQTypes
import Database.PostgreSQL.PQTypes.Internal.Oid
import Test.Env
import Test.QuickCheck.Arbitrary.Instances ()

rowDecoderTests :: TestData -> [TestTree]
rowDecoderTests td =
  [ decoderErrorTest td
  , rowDecoderTest td
  ]

data Simple = Simple (Maybe Int32) (Maybe Day)
  deriving stock (Eq, Generic, Ord, Show)

instance FromSQL Simple where
  fromSQL = decodeComposite genericDecoder

instance Arbitrary Simple where
  arbitrary = Simple <$> arbitrary <*> arbitrary

data Nested = Nested (Maybe Double) (Maybe Simple)
  deriving stock (Eq, Generic, Ord, Show)

instance FromSQL Nested where
  fromSQL = decodeComposite genericDecoder

instance Arbitrary Nested where
  arbitrary = Nested <$> arbitrary <*> arbitrary

-- | Deliberately has no 'FromSQL' instance: decoded with 'genericDecoder'.
data NoInstance = NoInstance (Maybe Int32) (Maybe Day)
  deriving stock (Eq, Generic, Show)

decoderErrorTest :: TestData -> TestTree
decoderErrorTest td =
  testGroup
    "Decoder errors"
    [ typeMismatch
    , tooFewColumnsConsumed
    , tooManyColumnsWanted
    ]
  where
    typeMismatch = testCase "Type mismatch is detected" $ do
      runTestEnv td defaultTransactionSettings $ do
        runSQL_ "SELECT 1::int4"
        expectError @TypeMismatch "int4 decoded as text" check $
          fetchOne (fromSQL @T.Text)
      where
        check TypeMismatch {..} = do
          assertEqual "Expected OID is correct" textOid tmExpectedOid
          assertEqual "Delivered OID is correct" int4Oid tmDeliveredOid

    tooFewColumnsConsumed = testCase "Not consuming all columns is detected" $ do
      runTestEnv td defaultTransactionSettings $ do
        runSQL_ "SELECT 1::int4, 2::int4"
        expectError @RowLengthMismatch "one of two columns consumed" check $
          fetchOne (fromSQL @Int32)
      where
        check RowLengthMismatch {..} = do
          assertEqual "Expected length is correct" 1 lengthExpected
          assertEqual "Delivered length is correct" 2 lengthDelivered

    tooManyColumnsWanted = testCase "Consuming too many columns is detected" $ do
      runTestEnv td defaultTransactionSettings $ do
        runSQL_ "SELECT 1::int4"
        expectError @RowLengthMismatch "two of one columns wanted" check $
          fetchOne ((,) <$> fromSQL @Int32 <*> fromSQL @Int32)
      where
        check RowLengthMismatch {..} = do
          assertEqual "Expected length is correct" 2 lengthExpected
          assertEqual "Delivered length is correct" 1 lengthDelivered

rowDecoderTest :: TestData -> TestTree
rowDecoderTest td =
  testGroup
    "Row decoder"
    [ anonymousRecordWorks
    , namedCompositeWorks
    , simpleRoundtrips
    , nestedRoundtrips
    , genericFromSQLWorks
    , nullRowWorks
    , rowOnScalarFails
    , partialRecordConsumptionFails
    , excessiveRecordConsumptionFails
    , differentCompositeFails
    ]
  where
    anonymousRecordWorks = testCase "Anonymous record decoding works" $ do
      runTestEnv td defaultTransactionSettings $ do
        runSQL_ "SELECT 1::int4, (2::int8, 'hi'::text), true"
        result <-
          fetchOne $
            (,,)
              <$> fromSQL @Int32
              <*> decodeComposite ((,) <$> fromSQL @Int64 <*> fromSQL @T.Text)
              <*> fromSQL @Bool
        assertEqual "Result is correct" (1, (2, "hi"), True) result

    simpleRoundtrips = testCase "Simple composite roundtrips correctly" $ do
      runTestEnv td defaultTransactionSettings . runTimes 100 $ do
        s@(Simple a b) <- randomValue 100
        runQuery_ $ rawSQL "SELECT ($1, $2)" (a, b)
        s' <- fetchOne $ fromSQL @Simple
        assertEqual "Simple doesn't change after getting through database" s s'

    nestedRoundtrips = testCase "Nested composite roundtrips correctly" $ do
      runTestEnv td defaultTransactionSettings . runTimes 100 $ do
        n@(Nested d ms) <- randomValue 100
        case ms of
          Nothing ->
            runQuery_ $ rawSQL "SELECT ($1, NULL)" (Identity d)
          Just (Simple a b) ->
            runQuery_ $ rawSQL "SELECT ($1, ($2, $3))" (d, a, b)
        n' <- fetchOne $ fromSQL @Nested
        assertEqual "Nested doesn't change after getting through database" n n'

    genericFromSQLWorks = testCase "Generic composite decoding works" $ do
      runTestEnv td defaultTransactionSettings $ do
        runSQL_ "SELECT (1, '2024-01-01'::date)::simple_"
        -- NoInstance has no FromSQL instance, the decoder comes solely from
        -- its Generic instance.
        result <- fetchOne . decodeComposite $ genericDecoder @NoInstance
        assertEqual
          "Result is correct"
          (NoInstance (Just 1) (Just $ fromGregorian 2024 1 1))
          result

    namedCompositeWorks = testCase "Named composite decoding works" $ do
      runTestEnv td defaultTransactionSettings $ do
        -- The casts exercise decoding of user-defined composite types, whose
        -- OIDs differ from the OID of the record pseudo-type.
        runSQL_ "SELECT (1, NULL)::simple_, (0.5, (2, '2024-01-01'::date)::simple_)::nested_"
        result <- fetchOne $ (,) <$> fromSQL @Simple <*> fromSQL @Nested
        assertEqual
          "Result is correct"
          ( Simple (Just 1) Nothing
          , Nested (Just 0.5) . Just $ Simple (Just 2) (Just $ fromGregorian 2024 1 1)
          )
          result

    nullRowWorks = testCase "NULL composite decodes to Nothing" $ do
      runTestEnv td defaultTransactionSettings $ do
        runSQL_ "SELECT NULL::record"
        ms <- fetchOne $ fromSQL @(Maybe Simple)
        assertEqual "NULL record decoded correctly" Nothing ms

    rowOnScalarFails = testCase "Row decoder fails on a scalar field" $ do
      runTestEnv td defaultTransactionSettings $ do
        runSQL_ "SELECT 1::int4"
        expectError @TypeMismatch "row on int4" check $
          fetchOne (decodeComposite $ fromSQL @Int32)
      where
        check TypeMismatch {..} = do
          assertEqual "Expected OID is correct" recordOid tmExpectedOid
          assertEqual "Delivered OID is correct" int4Oid tmDeliveredOid

    partialRecordConsumptionFails = testCase "Not consuming all fields of a record is detected" $ do
      runTestEnv td defaultTransactionSettings $ do
        runSQL_ "SELECT (1::int4, 2::int4)"
        expectError @RowLengthMismatch "one of two record fields consumed" check $
          fetchOne (decodeComposite $ fromSQL @Int32)
      where
        check RowLengthMismatch {..} = do
          assertEqual "Expected length is correct" 1 lengthExpected
          assertEqual "Delivered length is correct" 2 lengthDelivered

    excessiveRecordConsumptionFails = testCase "Consuming too many fields of a record is detected" $ do
      runTestEnv td defaultTransactionSettings $ do
        runSQL_ "SELECT (1, '2024-01-01'::date)::simple_"
        expectError @RowLengthMismatch "three fields wanted from a two-field composite" check
          . fetchOne
          . decodeComposite
          $ (,,)
            <$> fromSQL @(Maybe Int32)
            <*> fromSQL @(Maybe Day)
            <*> fromSQL @(Maybe Int32)
      where
        check RowLengthMismatch {..} = do
          assertEqual "Expected length is correct" 3 lengthExpected
          assertEqual "Delivered length is correct" 2 lengthDelivered

    differentCompositeFails = testCase "Field type mismatch in a composite is detected" $ do
      runTestEnv td defaultTransactionSettings $ do
        -- The field types of nested_ (float8, simple_) don't match the field
        -- types of Simple (int4, date), which the decoder of the first field
        -- detects.
        runSQL_ "SELECT (0.5, (2, '2024-01-01'::date)::simple_)::nested_"
        expectError @TypeMismatch "nested_ decoded as Simple" check $
          fetchOne (fromSQL @Simple)
      where
        check TypeMismatch {..} = do
          assertEqual "Expected OID is correct" int4Oid tmExpectedOid
          assertEqual "Delivered OID is correct" float8Oid tmDeliveredOid
