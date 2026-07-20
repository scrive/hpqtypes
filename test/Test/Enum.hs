-- | Tests of the enum deriving helpers.
module Test.Enum
  ( enumTests
  ) where

import Control.Monad
import Data.Int
import Data.Text qualified as T
import Test.Tasty

import Database.PostgreSQL.PQTypes
import Database.PostgreSQL.PQTypes.Internal.Oid
import Test.Env

enumTests :: TestData -> [TestTree]
enumTests td = [enumTest td]

data Colours = Blue | Black | Red | Mauve | Orange
  deriving stock (Eq, Show, Enum, Bounded)
  deriving (PQFormat, ToSQL, FromSQL) via SQLEnum Colours

instance EnumEncodingAs Int16 Colours where
  encodeEnumAs = \case
    Blue -> 1
    Black -> 7
    Red -> 2
    Mauve -> 6
    Orange -> 3

data Person = Alfred | Bertrand | Charles
  deriving stock (Eq, Show, Enum, Bounded)
  deriving (PQFormat, ToSQL, FromSQL) via SQLEnumAsText Person

instance EnumEncodingAs T.Text Person where
  encodeEnumAs = \case
    Alfred -> "alfred"
    Bertrand -> "bertrand"
    Charles -> "charles"

enumTest :: TestData -> TestTree
enumTest td = testCase "Enum deriving helpers work"
  . runTestEnv td defaultTransactionSettings
  $ do
    forM_ [minBound .. maxBound] $ \(c :: Colours) -> do
      runSQL_ $ "SELECT" <?> c
      c' <- fetchOne fromSQL
      assertEqual "Colour roundtrips correctly" c c'
    runSQL_ "SELECT 42::int2"
    expectError @(RangeError Int16) "invalid enum value" checkRange $
      fetchOne (fromSQL @Colours)
    -- The RangeError is wrapped in a ConversionError carrying the position
    -- of the field (courtesy of the MonadThrow instance of RowDecoder).
    runSQL_ "SELECT 42::int2"
    expectError @ConversionError "enum error carries position" checkRangePosition $
      fetchOne (fromSQL @Colours)

    -- Values of actual PostgreSQL enum types can be inserted, compared with
    -- and fetched without any casts.
    forM_ [minBound .. maxBound] $ \(p :: Person) -> do
      runQuery_ $ rawSQL "INSERT INTO people_ (p) VALUES ($1)" (Identity p)
    runSQL_ "SELECT p FROM people_ ORDER BY p"
    people <- fetchMany (fromSQL @Person)
    assertEqual "People roundtrip correctly" [minBound .. maxBound] people
    n <- runQuery $ rawSQL "SELECT TRUE FROM people_ WHERE p = $1" (Identity Bertrand)
    assertEqual "Number of Bertrands is correct" 1 n

    -- Arrays of text enums are sent as text[]: contexts without type
    -- information work, but usage against enum columns needs a cast.
    runQuery_ $ rawSQL "SELECT $1" (Identity [Alfred, Charles])
    people' <- fetchOne (fromSQL @[Person])
    assertEqual "Array of people roundtrips correctly" [Alfred, Charles] people'
    nArr <-
      runQuery $
        rawSQL "SELECT TRUE FROM people_ WHERE p = ANY($1::person_[])" (Identity [Alfred, Bertrand])
    assertEqual "Number of people in the array is correct" 2 nArr
    runSQL_ "SELECT 'batman'::text"
    expectError @(InvalidValue T.Text) "invalid enum text" checkInvalid $
      fetchOne (fromSQL @Person)
    runSQL_ "SELECT 1::int4"
    expectError @TypeMismatch "enum decoder on int4" checkNonEnum $
      fetchOne (fromSQL @Person)
    rollback
  where
    checkRange RangeError {..} = do
      assertEqual "Invalid value is correct" 42 reValue
      assertEqual "Valid ranges are correct" [(1, 3), (6, 7)] reRange
    checkRangePosition ConversionError {convColumn = col, convRow = row} = do
      assertEqual "Column is correct" 1 col
      assertEqual "Row is correct" 1 row
    checkInvalid InvalidValue {..} = do
      assertEqual "Invalid value is correct" "batman" ivValue
      assertEqual
        "Valid values are correct"
        (Just ["alfred", "bertrand", "charles"])
        ivValidValues
    checkNonEnum TypeMismatch {..} = do
      assertEqual "Expected OID is correct" textOid tmExpectedOid
      assertEqual "Delivered OID is correct" int4Oid tmDeliveredOid
