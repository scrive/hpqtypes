{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
import Control.Monad
import Data.Char

import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Program.Find
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
, cleanHook = \desc unit userHooks flags -> do
  cleanHook simpleUserHooks desc unit userHooks flags
  pqTypesDistclean (fromFlag $ cleanVerbosity flags)
}

pgconfigProgram :: Program
pgconfigProgram = (simpleProgram "pgconfig") {
  programFindLocation = \verbosity psp -> do
    pgconfig  <- findProgramOnSearchPath verbosity psp "pgconfig"
    pg_config <- findProgramOnSearchPath verbosity psp "pg_config"
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

pqTypesDistclean :: Verbosity -> IO ()
pqTypesDistclean verbosity =
  rawSystemExit verbosity "env" ["make", "--directory=libpqtypes", "distclean"]
