-- | Data shared by all the tests.
module Test.Env
  ( TestData (..)
  , runTest
  ) where

import Effectful
import Effectful.Concurrent
import Effectful.HPQTypes

newtype TestData = TestData
  { tdConnSource :: forall es. IOE :> es => ConnectionSourceM (Eff es)
  }

runTest :: TestData -> Eff [DB, Concurrent, IOE] a -> IO a
runTest td =
  runEff . runConcurrent . runDB (tdConnSource td) defaultTransactionSettings
