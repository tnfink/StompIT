module Paths_stomp_lib (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/tnfink/Library/Haskell/ghc-7.0.3/lib/stomp-lib-0.1/bin"
libdir     = "/Users/tnfink/Library/Haskell/ghc-7.0.3/lib/stomp-lib-0.1/lib"
datadir    = "/Users/tnfink/Library/Haskell/ghc-7.0.3/lib/stomp-lib-0.1/share"
libexecdir = "/Users/tnfink/Library/Haskell/ghc-7.0.3/lib/stomp-lib-0.1/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "stomp_lib_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "stomp_lib_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "stomp_lib_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "stomp_lib_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)