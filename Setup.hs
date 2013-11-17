{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
import Control.Monad
import Data.Char

import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.Verbosity
import System.FilePath
import System.Directory
import System.Exit

main :: IO ()
main = defaultMainWithHooks simpleUserHooks {
  hookedPrograms = [pgconfigProgram]
, confHook = \pkg flags -> do
  lbi <- confHook simpleUserHooks pkg flags
  let verbosity = fromFlag $ configVerbosity flags
  pqTypesConfigure verbosity
  bi <- psqlBuildInfo verbosity lbi
  return lbi {
    localPkgDescr = updatePackageDescription (Just bi, []) (localPkgDescr lbi)
  }
}

pgconfigProgram :: Program
pgconfigProgram = (simpleProgram "pgconfig") {
  programFindLocation = \verbosity -> constOrId $ do
    pgconfig  <- findProgramLocation verbosity "pgconfig"
    pg_config <- findProgramLocation verbosity "pg_config"
    return $ pgconfig `mplus` pg_config
}

psqlBuildInfo :: Verbosity -> LocalBuildInfo -> IO BuildInfo
psqlBuildInfo verbosity lbi = do
  (pgconfigProg, _) <- requireProgram verbosity pgconfigProgram (withPrograms lbi)
  let pgconfig = rawSystemProgramStdout verbosity pgconfigProg

  incDir <- pgconfig ["--includedir"]
  libDir <- pgconfig ["--libdir"]

  return emptyBuildInfo {
    extraLibDirs = [trim libDir]
  , includeDirs  = [trim incDir]
  }
  where
    trim = let f = reverse . dropWhile isSpace in f . f

pqTypesConfigure :: Verbosity -> IO ()
pqTypesConfigure verbosity = do
  dir <- getCurrentDirectory
  setCurrentDirectory $ dir </> "libpqtypes"
  -- run configure to create appropriate pqt_config.h
  res <- rawSystemExitCode verbosity "env" ["./configure"]
  case res of
    ExitFailure _ -> error "libpqtypes configure failed"
    _             -> setCurrentDirectory dir

--------------------

-- 'ConstOrId' is a @Cabal-1.16@ vs @Cabal-1.18@ compatibility hack,
-- 'programFindLocation' has a new (unused in this case)
-- parameter. 'ConstOrId' adds this parameter when types say it is
-- mandatory.
class ConstOrId a b where
    constOrId :: a -> b

instance ConstOrId a a where
    constOrId = id

instance ConstOrId a (b -> a) where
    constOrId = const
