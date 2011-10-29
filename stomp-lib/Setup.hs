import Distribution.Simple
import Distribution.PackageDescription(PackageDescription)
import Distribution.Simple.LocalBuildInfo(LocalBuildInfo)
import System.Cmd(system)
import Distribution.Simple.LocalBuildInfo

-- main = defaultMain

main = defaultMainWithHooks 
  (simpleUserHooks {runTests = runStompLibTests})
  
runStompLibTests:: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
runStompLibTests args b packageDescriptions localBuildInfo = 
  system ( "runhaskell ./src/Stomp.hs") >> return()

