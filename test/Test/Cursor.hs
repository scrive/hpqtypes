-- | Tests of cursor support.
module Test.Cursor
  ( cursorTests
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Data.Function
import Data.Int
import Data.Typeable
import Test.Tasty

import Data.Monoid.Utils
import Database.PostgreSQL.PQTypes
import Test.Env

cursorTests :: TestData -> [TestTree]
cursorTests td = [cursorTest td]

cursorTest :: TestData -> TestTree
cursorTest td =
  testGroup
    "Cursors"
    [ basicCursorWorks
    , scrollableCursorWorks
    , withHoldCursorWorks
    , doubleCloseWorks
    , cleanupDoesNotMaskErrors
    ]
  where
    basicCursorWorks = testCase "Basic cursor works" $ do
      runTestEnv td defaultTransactionSettings $ do
        withCursor "ints" NoScroll NoHold (sqlGenInts 5) $ \cursor -> do
          xs <- (`fix` []) $ \loop acc ->
            cursorFetch cursor CD_Next >>= \case
              0 -> pure $ reverse acc
              1 -> do
                n <- fetchOne $ fromSQL @Int32
                loop $ n : acc
              n -> error $ "Unexpected number of rows: " ++ show n
          assertEqual "Data fetched correctly" [1 .. 5] xs

    scrollableCursorWorks = testCase "Cursor declared as SCROLL works" $ do
      runTestEnv td defaultTransactionSettings $ do
        withCursor "ints" Scroll NoHold (sqlGenInts 10) $ \cursor -> do
          checkMove cursor CD_Next 1
          checkMove cursor CD_Prior 0
          checkMove cursor CD_First 1
          checkMove cursor CD_Last 1
          checkMove cursor CD_Backward_All 9
          checkMove cursor CD_Forward_All 10
          checkMove cursor (CD_Absolute 0) 0
          checkMove cursor (CD_Relative 0) 0
          checkMove cursor (CD_Forward 5) 5
          checkMove cursor (CD_Backward 5) 4

          cursorFetch_ cursor CD_Forward_All
          xs1 <- fetchMany $ fromSQL @Int32
          assertEqual "xs1 is correct" [1 .. 10] xs1
          cursorFetch_ cursor CD_Backward_All
          xs2 <- fetchMany $ fromSQL @Int32
          assertEqual "xs2 is correct" (reverse [1 .. 10]) xs2
      where
        checkMove cursor cd expected = do
          moved <- cursorMove cursor cd
          assertEqual
            ( "Moving cursor with"
                <+> show cd
                <+> "would fetch a correct amount of rows"
            )
            expected
            moved

    withHoldCursorWorks = testCase "Cursor declared as WITH HOLD works" $ do
      runTestEnv td defaultTransactionSettings . unsafeWithoutTransaction $ do
        withCursor "ints" NoScroll Hold (sqlGenInts 10) $ \cursor -> do
          cursorFetch_ cursor CD_Forward_All
          rows <- ntuples <$> queryResult
          assertEqual "Number of rows is correct" 10 rows
          sum_ <- foldlDB (fromSQL @Int32) (\acc n -> pure $ acc + n) 0
          assertEqual "sum_ is correct" 55 sum_

    doubleCloseWorks = testCase "Double CLOSE works on a cursor" $ do
      runTestEnv td defaultTransactionSettings $ do
        withCursorSQL "ints" NoScroll NoHold "SELECT 1" $ \_cursor -> do
          -- Commiting a transaction closes the cursor
          commit

    cleanupDoesNotMaskErrors = testCase "Cursor cleanup doesn't mask the original error" $ do
      runTestEnv td defaultTransactionSettings $ do
        -- The failing query puts the transaction in the aborted state, in
        -- which closing the cursor fails with in_failed_sql_transaction. The
        -- original error needs to propagate regardless.
        eres <- try . withCursorSQL "ints" NoScroll NoHold (sqlGenInts 5) $ \_cursor -> do
          runSQL_ "SELECT 1/0"
        liftBase $ case eres :: Either DBException () of
          Left DBException {..}
            | Just DetailedQueryError {..} <- cast dbeError -> do
                assertEqual "Unexpected error code" DivisionByZero qeErrorCode
            | otherwise -> assertFailure $ "Unexpected exception: " ++ show dbeError
          Right () -> assertFailure "DBException wasn't thrown"
