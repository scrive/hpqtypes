{-# LANGUAGE NoFieldSelectors #-}

-- | The monad the tests run in, along with assertion helpers.
module Test.Env
  ( TestEnv (..)
  , TestData (..)
  , runTestEnv
  , randomValue
  , assertEqual
  , expectError
  , sqlGenInts
  , timed

    -- * Re-exports
  , module Test.Tasty.HUnit
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.State qualified as S
import Control.Monad.Trans.Control
import Data.Int
import GHC.Clock (getMonotonicTime)
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Test.Tasty.HUnit hiding (assertEqual)
import Test.Tasty.HUnit qualified as HUnit

import Data.Monoid.Utils
import Database.PostgreSQL.PQTypes

type InnerTestEnv = S.StateT QCGen (DBT IO)

newtype TestEnv a = TestEnv (InnerTestEnv a)
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadFail
    , MonadBase IO
    , MonadCatch
    , MonadDB
    , MonadMask
    , MonadThrow
    )

instance MonadBaseControl IO TestEnv where
  type StM TestEnv a = StM InnerTestEnv a
  liftBaseWith f = TestEnv $ liftBaseWith (\run -> f $ \(TestEnv m) -> run m)
  restoreM = TestEnv . restoreM

data TestData = TestData
  { gen :: !QCGen
  , connSettings :: !ConnectionSettings
  , connSource :: !(ConnectionSourceM IO)
  }

runTestEnv :: TestData -> TransactionSettings -> TestEnv a -> IO a
runTestEnv td ts (TestEnv m) = runDBT td.connSource ts $ S.evalStateT m td.gen

----------------------------------------

randomValue :: Arbitrary a => Int -> TestEnv a
randomValue n = do
  gen <- TestEnv $ S.state splitGen
  pure $ unGen arbitrary gen n

-- | 'HUnit.assertEqual' lifted to any 'MonadBase' 'IO' monad.
assertEqual :: (Eq a, Show a, MonadBase IO m) => String -> a -> a -> m ()
assertEqual preface expected actual =
  liftBase $ HUnit.assertEqual preface expected actual

-- | Find an error of a given type in a chain of arbitrarily nested
-- 'ConversionError's.
findNestedError :: forall e. Exception e => SomeException -> Maybe e
findNestedError err = case fromException err of
  Just e -> Just e
  Nothing
    | Just (ConversionError _ _ _ e) <- fromException err ->
        findNestedError $ toException e
    | otherwise -> Nothing

-- | Make sure that an action throws a 'DBException' with an error of a given
-- type inside. The error can be wrapped in 'ConversionError'.
expectError
  :: forall e a
   . Exception e
  => String
  -> (e -> TestEnv ())
  -> TestEnv a
  -> TestEnv ()
expectError what check action =
  try action >>= \case
    Left DBException {dbeError = err}
      | Just e <- findNestedError @e (toException err) -> check e
      | otherwise ->
          liftBase . assertFailure $
            "Unexpected exception (" ++ what ++ "): " ++ show err
    Right _ ->
      liftBase . assertFailure $
        "DBException wasn't thrown (" ++ what ++ ")"

----------------------------------------

sqlGenInts :: Int32 -> SQL
sqlGenInts n =
  smconcat
    [ "WITH RECURSIVE ints(n) AS"
    , "( VALUES (1) UNION ALL SELECT n+1 FROM ints WHERE n <" <?> n
    , ") SELECT n FROM ints"
    ]

-- | Run an action and return its result along with its duration in seconds.
timed :: MonadBase IO m => m a -> m (a, Double)
timed m = do
  t1 <- liftBase getMonotonicTime
  res <- m
  t2 <- liftBase getMonotonicTime
  pure (res, t2 - t1)
