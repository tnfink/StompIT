module Paths_stomp_testapplication (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/tnfink/.cabal/bin"
libdir     = "/Users/tnfink/.cabal/lib/stomp-testapplication-0.1/ghc-7.0.3"
datadir    = "/Users/tnfink/.cabal/share/stomp-testapplication-0.1"
libexecdir = "/Users/tnfink/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "stomp_testapplication_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "stomp_testapplication_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "stomp_testapplication_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "stomp_testapplication_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
