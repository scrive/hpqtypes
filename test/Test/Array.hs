{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Tests of the array decoder and the encoding of lists and vectors.
module Test.Array
  ( arrayTests
  ) where

import Control.Monad
import Data.ByteString qualified as BS
import Data.Int
import Data.Text qualified as T
import Data.Time
import Data.Vector qualified as V
import Test.QuickCheck (Arbitrary)
import Test.Tasty

import Data.Monoid.Utils
import Database.PostgreSQL.PQTypes
import Database.PostgreSQL.PQTypes.Internal.Oid
import Test.Env
import Test.RowDecoder (Simple (..))

arrayTests :: TestData -> [TestTree]
arrayTests td = [arrayDecoderTest td]

arrayDecoderTest :: TestData -> TestTree
arrayDecoderTest td =
  testGroup
    "Array decoder"
    [ arrayRoundtrips @Int32 "::int4[]" "Array of int4 roundtrips correctly"
    , arrayRoundtrips @T.Text "::text[]" "Array of text roundtrips correctly"
    , arrayRoundtrips @(Maybe Int32) "::int4[]" "Array with NULL elements roundtrips correctly"
    , nullArrayWorks
    , twoDimensionalArrayWorks
    , threeDimensionalArrayWorks
    , emptyMultiDimensionalArrayWorks
    , arrayOfRecordsWorks
    , arrayOfNullableRecordsWorks
    , listInstanceWorks
    , vectorDecodingWorks
    , elementTypeMismatchFails
    , vectorElementTypeMismatchFails
    , elementOverconsumptionFails
    , scalarDecoderOnTwoDimensionsFails
    , arrayDecoderOnOneDimensionFails
    , malformedDimensionsFail
    , nestedListsEncodeAsMultiDimensional
    , raggedArraysAreRejected
    ]
  where
    arrayOf :: [SQL] -> SQL -> SQL
    arrayOf xs castSql = "SELECT ARRAY[" <> mintercalate ", " xs <> "]" <> castSql

    arrayRoundtrips
      :: forall a
       . (Arbitrary a, Eq a, FromSQL a, Show a, ToSQL a)
      => SQL
      -> TestName
      -> TestTree
    arrayRoundtrips cast name = testCase name $ do
      runTestEnv td defaultTransactionSettings . replicateM_ 100 $ do
        xs <- randomValue @[a] 100
        runSQL_ $ arrayOf (map sqlParam xs) cast
        xs' <- fetchOne fromSQL
        assertEqual "Array doesn't change after getting through database" xs xs'

    nullArrayWorks = testCase "NULL array decodes to Nothing" $ do
      runTestEnv td defaultTransactionSettings $ do
        runSQL_ "SELECT NULL::int4[]"
        mxs <- fetchOne $ fromSQL @(Maybe [Int32])
        assertEqual "NULL array decoded correctly" Nothing mxs

    twoDimensionalArrayWorks = testCase "Two-dimensional array decoding works" $ do
      runTestEnv td defaultTransactionSettings $ do
        runSQL_ "SELECT '{{1,2,3},{4,5,6}}'::int4[]"
        xss <- fetchOne $ fromSQL @[[Int32]]
        assertEqual "Array decoded correctly" [[1, 2, 3], [4, 5, 6]] xss

    threeDimensionalArrayWorks = testCase "Three-dimensional array decoding works" $ do
      runTestEnv td defaultTransactionSettings $ do
        runSQL_ "SELECT '{{{1,2},{3,4}},{{5,6},{7,8}}}'::int4[]"
        xsss <- fetchOne $ fromSQL @[[[Int32]]]
        assertEqual
          "Array decoded correctly"
          [[[1, 2], [3, 4]], [[5, 6], [7, 8]]]
          xsss

    emptyMultiDimensionalArrayWorks = testCase "Empty multi-dimensional array decoding works" $ do
      runTestEnv td defaultTransactionSettings $ do
        runSQL_ "SELECT '{}'::int4[]"
        xss <- fetchOne $ fromSQL @[[Int32]]
        assertEqual "Array decoded correctly" [] xss

    arrayOfRecordsWorks = testCase "Array of composites decoding works" $ do
      runTestEnv td defaultTransactionSettings $ do
        runSQL_ "SELECT ARRAY[(1, '2024-01-01'::date), (NULL, NULL)]"
        xs <- fetchOne $ fromSQL @[Simple]
        assertEqual
          "Array decoded correctly"
          [ Simple (Just 1) (Just $ fromGregorian 2024 1 1)
          , Simple Nothing Nothing
          ]
          xs

    arrayOfNullableRecordsWorks = testCase "Array of nullable composites decoding works" $ do
      runTestEnv td defaultTransactionSettings $ do
        runSQL_ "SELECT ARRAY[NULL, (2, NULL)]"
        xs <- fetchOne $ fromSQL @[Maybe Simple]
        assertEqual
          "Array decoded correctly"
          [Nothing, Just $ Simple (Just 2) Nothing]
          xs

    listInstanceWorks = testCase "FromSQL instance for lists works" $ do
      runTestEnv td defaultTransactionSettings $ do
        runSQL_ "SELECT '{1,2,3}'::int4[], '{{1,2},{3,4}}'::int4[], 'hello'::text, '{foo,bar}'::text[]"
        result <- fetchOne $ (,,,) <$> fromSQL <*> fromSQL <*> fromSQL <*> fromSQL
        assertEqual
          "Result is correct"
          ( [1, 2, 3] :: [Int32]
          , [[1, 2], [3, 4]] :: [[Int32]]
          , "hello" :: String
          , ["foo", "bar"] :: [String]
          )
          result

    elementTypeMismatchFails = testCase "Type mismatch of array elements is detected" $ do
      runTestEnv td defaultTransactionSettings $ do
        runSQL_ "SELECT '{1,2}'::int4[]"
        expectError @TypeMismatch "int4 elements decoded as int8" check $
          fetchOne (fromSQL @[Int64])
      where
        check TypeMismatch {..} = do
          assertEqual "Expected OID is correct" int8Oid tmExpectedOid
          assertEqual "Delivered OID is correct" int4Oid tmDeliveredOid

    vectorDecodingWorks = testCase "Vector decoding works" $ do
      runTestEnv td defaultTransactionSettings $ do
        runSQL_ "SELECT '{1,2,3}'::int4[], '{{1,2},{3,4}}'::int4[], '{1,NULL}'::int4[], ARRAY[(2, NULL)]"
        result <- fetchOne $ (,,,) <$> fromSQL <*> fromSQL <*> fromSQL <*> fromSQL
        assertEqual
          "Result is correct"
          ( V.fromList [1, 2, 3 :: Int32]
          , V.fromList [V.fromList [1, 2], V.fromList [3, 4 :: Int32]]
          , V.fromList [Just 1, Nothing :: Maybe Int32]
          , V.fromList [Simple (Just 2) Nothing]
          )
          result

    vectorElementTypeMismatchFails = testCase "Type mismatch of vector elements is detected" $ do
      runTestEnv td defaultTransactionSettings $ do
        runSQL_ "SELECT '{1,2}'::int4[]"
        expectError @TypeMismatch "int4 elements decoded as int8" check $
          fetchOne (fromSQL @(V.Vector Int64))
      where
        check TypeMismatch {..} = do
          assertEqual "Expected OID is correct" int8Oid tmExpectedOid
          assertEqual "Delivered OID is correct" int4Oid tmDeliveredOid

    elementOverconsumptionFails = testCase "Consuming more than one field per array element is detected" $ do
      runTestEnv td defaultTransactionSettings $ do
        runSQL_ "SELECT '{1,2}'::int4[]"
        expectError @RowLengthMismatch "two fields consumed per element" check $
          fetchOne (decodeArray $ (,) <$> fromSQL @Int32 <*> fromSQL @Int32)
      where
        check RowLengthMismatch {..} = do
          assertEqual "Expected length is correct" 2 lengthExpected
          assertEqual "Delivered length is correct" 1 lengthDelivered

    scalarDecoderOnTwoDimensionsFails = testCase "Scalar decoder fails on a two-dimensional array" $ do
      runTestEnv td defaultTransactionSettings $ do
        runSQL_ "SELECT '{{1},{2}}'::int4[]"
        expectError @ArrayDimensionMismatch "scalar decoder on 2-dim array" check $
          fetchOne (fromSQL @[Int32])
      where
        check ArrayDimensionMismatch {..} = do
          assertEqual "Expected dimension is correct" 0 arrDimExpected
          assertEqual "Delivered dimension is correct" 1 arrDimDelivered

    arrayDecoderOnOneDimensionFails = testCase "Nested array decoder fails on a one-dimensional array" $ do
      runTestEnv td defaultTransactionSettings $ do
        runSQL_ "SELECT '{1,2}'::int4[]"
        expectError @HPQTypesError "array decoder on int4 elements" (\_ -> pure ()) $
          fetchOne (fromSQL @[[Int32]])

    malformedDimensionsFail = testCase "Malformed array dimensions are rejected" $ do
      runTestEnv td defaultTransactionSettings $ do
        -- A header claiming dimensions of sizes 10^9 and 0: the number of
        -- elements is 0, so the lack of element data is consistent, but
        -- accepting the sizes would make the decoder allocate 10^9
        -- sub-arrays.
        let zeroDim =
              BS.pack $
                concat
                  [ [0, 0, 0, 2] -- 2 dimensions
                  , [0, 0, 0, 0] -- no NULLs
                  , [0, 0, 0, 23] -- int4 elements
                  , [59, 154, 202, 0, 0, 0, 0, 1] -- size 10^9, lower bound 1
                  , [0, 0, 0, 0, 0, 0, 0, 1] -- size 0, lower bound 1
                  ]
        runQuery_ $ rawSQL "SELECT $1::bytea" (Identity zeroDim)
        expectError @HPQTypesError "dimension of size 0" (\_ -> pure ()) $
          fetchOne (fromSQL @[[Int32]])
        -- A header claiming three dimensions of size 2^32 - 1: the number of
        -- elements exceeds the maximum the server can send (and overflows
        -- Int64).
        let hugeDims =
              BS.pack $
                concat
                  [ [0, 0, 0, 3] -- 3 dimensions
                  , [0, 0, 0, 0] -- no NULLs
                  , [0, 0, 0, 23] -- int4 elements
                  , concat (replicate 3 [255, 255, 255, 255, 0, 0, 0, 1])
                  ]
        runQuery_ $ rawSQL "SELECT $1::bytea" (Identity hugeDims)
        expectError @HPQTypesError "number of elements too large" (\_ -> pure ()) $
          fetchOne (fromSQL @[[[Int32]]])

    nestedListsEncodeAsMultiDimensional = testCase "Nested lists and vectors encode as multi-dimensional arrays" $ do
      runTestEnv td defaultTransactionSettings $ do
        let roundtrip :: (Eq a, Show a, FromSQL a, ToSQL a) => String -> a -> TestEnv ()
            roundtrip dims v = do
              runQuery_ $ rawSQL "SELECT array_dims($1), $1" (Identity v)
              result <- fetchOne ((,) <$> fromSQL @String <*> fromSQL)
              assertEqual "Dimensions and value are correct" (dims, v) result
        roundtrip "[1:2][1:3]" [[1, 2, 3], [4, 5, 6 :: Int32]]
        roundtrip "[1:2][1:2]" . V.fromList $
          [ V.fromList [Just 1, Nothing]
          , V.fromList [Nothing, Just (4 :: Int32)]
          ]
        roundtrip "[1:2][1:2][1:2]" [[[1, 2], [3, 4]], [[5, 6], [7, 8 :: Int32]]]
        roundtrip "[1:2][1:2]" [V.fromList [1, 2], V.fromList [3, 4 :: Int32]]
        roundtrip "[1:3]" ["foo", "bar", "baz" :: String]
        roundtrip "[1:2][1:2]" [["foo", "bar"], ["baz", "qux" :: String]]

    raggedArraysAreRejected = testCase "Ragged multi-dimensional arrays are rejected before sending" $ do
      runTestEnv td defaultTransactionSettings $ do
        let rejected :: (Show a, ToSQL a) => String -> a -> TestEnv ()
            rejected what v =
              expectError @HPQTypesError what (\_ -> pure ())
                . runQuery_
                $ rawSQL "SELECT $1" (Identity v)
        -- The dangerous case: the number of elements matches the dimensions
        -- taken from the last sub-list, so without the client-side check the
        -- server would silently store a reshaped array.
        rejected "sub-lists of different lengths" [[1, 2, 3], [4], [5, 6 :: Int32]]
        rejected "sub-vectors of different lengths" . V.fromList $
          [V.fromList [1, 2], V.fromList [3 :: Int32]]
        -- Raggedness one level down within a single branch.
        rejected "deeper ragged sub-lists" [[[1, 2], [3, 4, 5 :: Int32]]]
        -- Raggedness visible only across branches.
        rejected "raggedness across branches" [[[1, 2]], [[3, 4, 5 :: Int32]]]
        -- Raggedness visible only across branches, with the number of
        -- elements matching the dimensions taken from the last branch
        -- ([3, 2, 3]): sub-arrays are uniform among their immediate
        -- siblings, so only comparing their full dimensions catches this
        -- silent reshape.
        rejected
          "cross-branch raggedness with matching element count"
          [ [[1, 2], [3, 4]]
          , [[5, 6, 7, 8], [9, 10, 11, 12]]
          , [[13, 14, 15], [16, 17, 18 :: Int32]]
          ]
        -- Mixed nesting.
        rejected "ragged vector of lists" . V.fromList $ [[1, 2], [3 :: Int32]]
        -- A NULL sub-array (which the server cannot represent either).
        rejected "NULL sub-array" [Just [1, 2], Nothing, Just [3, 4 :: Int32]]
        rejected "ragged lists of strings" [["foo", "bar"], ["baz" :: String]]
        -- Strings are scalar elements: their lengths are not dimensions.
        let strss = [["foo", "a"], ["quux", "bc" :: String]]
        runQuery_ $ rawSQL "SELECT $1" (Identity strss)
        strss' <- fetchOne fromSQL
        assertEqual "String lengths are not dimensions" strss strss'
