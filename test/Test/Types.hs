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
import Data.IP (IP, IPRange)
import Data.Int
import Data.List qualified as L
import Data.Scientific
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time
import Data.Typeable
import Data.UUID.Types qualified as U
import Data.Vector qualified as V
import Data.Word
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit qualified as HUnit
import TextShow

import Data.Monoid.Utils
import Database.PostgreSQL.PQTypes
import Database.PostgreSQL.PQTypes.Internal.Decoding qualified as D
import Database.PostgreSQL.PQTypes.Internal.Oid
import Test.Env
import Test.QuickCheck.Arbitrary.Instances

typesTests :: TestData -> [TestTree]
typesTests td =
  [ uuidTest td
  , integerTest td
  , fractionalNumericTest td
  , xmlTest td
  , nulInTextTest td
  , numericLimitsTest td
  , rangeBoundLengthTest
  , inetTest td
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
  , nullTest @IP td
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
  , putGetTest @IP td 100
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

-- | @inet@ maps to 'IP' and @cidr@ to 'IPRange'. The wire format of an
-- @inet@ carries the host bits of the address along with the length of its
-- netmask, so a bare 'IP' can only represent the values whose netmask covers
-- the whole address.
inetTest :: TestData -> TestTree
inetTest td = testCase "inet maps to IP and cidr to IPRange"
  . runTestEnv td defaultTransactionSettings
  $ do
    -- The host bits used to be discarded, turning this into 10.0.0.0/8.
    runSQL_ "SELECT '10.0.0.5/8'::inet::text, '2001:db8::5/32'::inet::text"
    addresses <- fetchOne ((,) <$> fromSQL @T.Text <*> fromSQL @T.Text)
    assertEqual "Host bits survive on the server" ("10.0.0.5/8", "2001:db8::5/32") addresses

    -- A netmask narrower than the address is rejected rather than dropped.
    runSQL_ "SELECT '10.0.0.5/8'::inet"
    expectError @HPQTypesError "netmask on an IP" checkNetmask $ fetchOne (fromSQL @IP)

    -- A bare address has a netmask covering it, so it decodes.
    runSQL_ "SELECT '10.0.0.5'::inet"
    addr <- fetchOne $ fromSQL @IP
    assertEqual "Bare address decodes" (read "10.0.0.5") addr

    -- The OIDs keep the two types apart in both directions.
    runSQL_ "SELECT '10.0.0.0/8'::cidr"
    expectError @TypeMismatch "cidr as IP" (checkOids inetOid cidrOid) $ fetchOne (fromSQL @IP)
    runSQL_ "SELECT '10.0.0.5'::inet"
    expectError @TypeMismatch "inet as IPRange" (checkOids cidrOid inetOid) $
      fetchOne (fromSQL @IPRange)
  where
    checkNetmask (HPQTypesError msg) =
      liftBase . assertBool ("Error mentions the netmask: " ++ msg) $
        "/8" `L.isInfixOf` msg
    checkOids expected delivered TypeMismatch {..} = do
      assertEqual "Expected OID is correct" expected tmExpectedOid
      assertEqual "Delivered OID is correct" delivered tmDeliveredOid

-- | The header fields of a @numeric@ value are 16 bit wide, so a large
-- enough exponent doesn't fit. The upstream encoder these are derived from
-- lets such a value wrap around, which silently stores a different number
-- (@1e131072@ used to arrive as @0@).
numericLimitsTest :: TestData -> TestTree
numericLimitsTest td = testCase "Numeric values that don't fit the wire format are rejected"
  . runTestEnv td defaultTransactionSettings
  $ do
    -- The largest weight and scale the format can represent still work.
    roundtrips "largest weight" $ scientific 1 131068
    roundtrips "largest scale" $ scientific 1 (-16380)
    rejected "weight" $ scientific 1 131072
    rejected "weight" $ scientific 1 200000
    rejected "scale" $ scientific 1 (-100000)
  where
    roundtrips what v = do
      runQuery_ $ rawSQL "SELECT $1" (Identity v)
      v' <- fetchOne fromSQL
      assertEqual ("Value with the " ++ what ++ " roundtrips") v v'

    rejected what v =
      expectError @HPQTypesError ("numeric " ++ what ++ " out of range") check
        . runQuery_
        $ rawSQL "SELECT $1" (Identity v)
      where
        check (HPQTypesError msg) =
          liftBase . assertBool ("Error message mentions the " ++ what ++ ": " ++ msg) $
            what `L.isInfixOf` msg

-- | The length prefix of a value inside a container is signed, with @-1@
-- standing for NULL. The upstream decoder these are derived from passes any
-- other negative length on to @unsafeTake@ / @unsafeDrop@, which yields a
-- 'BS.ByteString' of negative length pointing before its buffer.
rangeBoundLengthTest :: TestTree
rangeBoundLengthTest = testCase "Negative length of a range bound is rejected" $ do
  HUnit.assertEqual
    "Well-formed range decodes"
    (Right $ Range (Incl 1) (Incl 5))
    (D.valueParser D.int4range wellFormed)
  case D.valueParser D.int4range negativeLength of
    Right r -> assertFailure $ "Malformed range decoded to " ++ show r
    Left err ->
      HUnit.assertBool ("Error mentions the length: " ++ T.unpack err) $
        "-24" `T.isInfixOf` err
  where
    -- Flags (lower and upper inclusive, neither infinite), then a bound
    -- claiming a length of 0xffffffe8, i.e. -24.
    negativeLength = BS.pack [0x06, 0xff, 0xff, 0xff, 0xe8, 0xde, 0xad, 0xbe, 0xef]
    wellFormed = BS.pack [0x06, 0, 0, 0, 4, 0, 0, 0, 1, 0, 0, 0, 4, 0, 0, 0, 5]

-- | The server rejects NUL characters in values of the character types, so
-- they need to be passed on verbatim for it to do so. The upstream encoders
-- the ones in "Database.PostgreSQL.PQTypes.Internal.Encoding" are derived
-- from silently drop such characters, which corrupts the value instead.
nulInTextTest :: TestData -> TestTree
nulInTextTest td = testCase "NUL characters in text values are rejected"
  . runTestEnv td defaultTransactionSettings
  $ do
    rejected "Text" CharacterNotInRepertoire $ Identity ("a\NULb" :: T.Text)
    rejected "lazy Text" CharacterNotInRepertoire $ Identity ("a\NULb" :: TL.Text)
    rejected "String" CharacterNotInRepertoire $ Identity ("a\NULb" :: String)
    rejected "array element" CharacterNotInRepertoire $ Identity ["a\NULb" :: T.Text]
    rejected "String array element" CharacterNotInRepertoire $ Identity [["a\NULb" :: String]]
    -- The XML parser rejects the character before the encoding check does.
    rejected "XML" InvalidXmlContent . Identity . XML $ "<a>x\NULy</a>"
  where
    rejected :: (Show row, ToRow row) => String -> ErrorCode -> row -> TestEnv ()
    rejected what expected row = do
      -- The receive function of the type of a parameter runs when the query
      -- is bound, so the value doesn't need to be used by the query itself.
      -- The savepoint keeps the transaction usable for the checks that follow.
      eres <- try . withSavepoint "nul" . runQuery_ $ rawSQL "SELECT $1" row
      liftBase $ case eres of
        Left DBException {..}
          | Just DetailedQueryError {..} <- cast dbeError ->
              assertEqual
                ("Unexpected error code (" ++ what ++ ")")
                expected
                qeErrorCode
          | otherwise ->
              assertFailure $ "Unexpected exception (" ++ what ++ "): " ++ show dbeError
        Right () ->
          assertFailure $ "NUL character wasn't rejected (" ++ what ++ ")"

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
