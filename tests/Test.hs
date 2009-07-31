module Test where

import System.Exit
import Test.HUnit
import qualified Test.POSet as POSet

main = runTestTT (TestList (POSet.tests)) >>= 
       \ counts -> exitWith (if errors counts /= 0 || failures counts /= 0 then ExitFailure 1 else ExitSuccess)
