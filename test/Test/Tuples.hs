-- | Tests of passing and fetching rows of parameters: generic tuple
-- decoding and row concatenation with ':++:'.
module Test.Tuples
  ( tupleTests
  ) where

import Data.ByteString qualified as BS
import Data.Int
import Data.Text qualified as T
import Data.Word
import Test.Tasty

import Database.PostgreSQL.PQTypes
import Test.Env
import Test.QuickCheck.Arbitrary.Instances

tupleTests :: TestData -> [TestTree]
tupleTests td =
  [ tupleDecoderTest td
  , rowConcatenationTest td
  ]

type Tuple10 =
  ( Int16
  , Int32
  , Int64
  , Double
  , Bool
  , AsciiChar
  , Word8
  , String0
  , BS.ByteString
  , T.Text
  )

tupleDecoderTest :: TestData -> TestTree
tupleDecoderTest td = testCase "Putting a 10-element tuple through database works" $ do
  runTestEnv td defaultTransactionSettings . runTimes 100 $ do
    t <- randomValue @Tuple10 100
    runQuery_ $ rawSQL "SELECT $1, $2, $3, $4, $5, $6, $7, $8, $9, $10" t
    t' <- fetchOne $ genericDecoder @Tuple10
    assertEqual "Tuple doesn't change after getting through database" t t'

-- | More than 10 parameters can be passed by combining rows with ':++:'.
rowConcatenationTest :: TestData -> TestTree
rowConcatenationTest td = testCase "Passing parameters of concatenated rows works" $ do
  runTestEnv td defaultTransactionSettings . runTimes 100 $ do
    t@(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) <- randomValue @Tuple10 100
    extra <- randomValue @(Int32, String0) 100
    runQuery_ $
      rawSQL
        "SELECT $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12"
        ((t1, t2, t3, t4, t5) :++: (t6, t7, t8, t9, t10) :++: extra)
    result <- fetchOne $ (:++:) <$> genericDecoder <*> genericDecoder
    assertEqual
      "Parameters don't change after getting through database"
      (t :++: extra)
      result
