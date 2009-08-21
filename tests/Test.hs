module Test where

import System.Exit
import Test.HUnit
import qualified Test.POSet as POSet
import qualified Test.Triplets as Triplets
import qualified Test.Merge as Merge

main = runTestTT (TestList (POSet.tests ++ Triplets.tests ++ Merge.tests)) >>= 
       \ counts -> exitWith (if errors counts /= 0 || failures counts /= 0 then ExitFailure 1 else ExitSuccess)
