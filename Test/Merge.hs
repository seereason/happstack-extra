{-# OPTIONS -fno-warn-missing-signatures #-}
module Test.Merge
    ( tests
    ) where

import Control.Applicative.Error
import qualified Data.ByteString.UTF8 as B
import Happstack.Data.IxSet.Merge
import Test.HUnit
-- import Debug.Trace

tests = [test1, test2, test3, test4, test5, test6, test7]

-- We want to know
--   3. Test conflicts
--   4. Test things that seem to conflict at the top level but end up merging lower down

-- Make sure strings don't get merged character by character
test1 = TestCase $ assertEqual "string conflict" (Failure ["stringFail: \"pup\", \"cup\", \"pun\""]) $
        -- trace "\n\nStarting test1" (return ()) >>
        threeWayMerge "pup" "cup" "pun"

-- Test the three types of merge - left edited
test2 = TestCase $ assertEqual "left edited" (Success "cup") $
        -- trace "\n\nStarting test2" (return ()) >>
        threeWayMerge "pup" "cup" "pup"

-- Right edited
test3 = TestCase $ assertEqual "right edited" (Success "cpu") $
        -- trace "\n\nStarting test3" (return ()) >>
        threeWayMerge "pup" "pup" "cpu"

-- Both edited but they match
test4 = TestCase $ assertEqual "matching edits" (Success "cap") $
        -- trace "\n\nStarting test4" (return ()) >>
        threeWayMerge "pup" "cap" "cap"

test5 = TestCase $ assertEqual "merging tuple" (Success (1, "hello", 2, "world") :: Failing (Int, String, Int, String)) $
        -- trace "\n\nStarting test5" (return ()) >> 
        threeWayMerge (1, "unedited", 2, "unedited") (1, "hello", 2, "unedited") (1, "unedited", 2, "world")

test6 = TestCase $ assertEqual "merging bytestrings" (Failure ["bsFail:\"pup\", \"cup\", \"pun\""]) $
        -- trace "\n\nStarting test6" (return ()) >> 
        threeWayMerge (B.fromString "pup") (B.fromString "cup") (B.fromString "pun")

test7 = TestCase $ assertEqual "merging bytestrings" (Success (B.fromString "cup")) $
        -- trace "\n\nStarting test7" (return ()) >> 
        threeWayMerge (B.fromString "pup") (B.fromString "cup") (B.fromString "pup")

-- test6 = TestCase $ assertEqual "conflicting tuple"
