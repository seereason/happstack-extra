import Distribution.Simple
import System.Cmd
import System.Exit

import Distribution.Simple
main = defaultMainWithHooks simpleUserHooks {
         postBuild = runTestScript
       , runTests = runTestScript
       }

runTestScript _args _flag _pd _lbi =
    system "runhaskell -isrc tests/Test.hs" >>=
    \ code -> if code == ExitSuccess then return () else error "Test Failure"
