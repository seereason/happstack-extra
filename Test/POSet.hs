{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Test.POSet
    ( tests
    ) where

import Happstack.Data.IxSet.POSet
import Test.HUnit
import qualified Data.Set as S

{-
        1
       / \
      2   3
     / \ / \
    4  5 6  7
   / \/ \
  8   9  10
-}

f :: Int -> [Int]
f 1 = []
f 2 = [1]
f 3 = [1]
f 4 = [2]
f 5 = [2]
f 6 = [3]
f 7 = [3]
f 8 = [4]
f 9 = [4, 5]
f 10 = [5]
f _ = []

instance POSet (Int -> [Int]) Int where
    parents f n = f n

posetTest1 = TestCase $ assertEqual "Test 1 of commonAncestor function" (Just 2) $
               commonAncestor f (8 :: Int) (10 :: Int)
posetTest2 = TestCase $ assertEqual "Test 2 of commonAncestor function" (Just 4) $
               commonAncestor f (8 :: Int) (9 :: Int)
posetTest3 = TestCase $ assertEqual "Test of commonAncestors function" (S.fromList [1, 2, 6, 8, 10]) $
               commonAncestors f [(8 :: Int), (6 :: Int), (10 :: Int)]
posetTest4 = TestCase $ assertEqual "Test of prune function" ([(2,[]),(8,[2]),(10,[2])], S.fromList [1,4,5]) $
               prune f (S.fromList ([2, 8, 10] :: [Int]))

tests :: [Test]
tests = [posetTest1, posetTest2, posetTest3, posetTest4]
