{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}
module Test.Triplets
    ( tests
    ) where

import Happstack.Data.IxSet.Triplets
import Data.Generics (Data, geq {-, gshow-})
import Test.HUnit
-- import Debug.Trace

tests :: [Test]
tests = [gzipTest1, gzipTest2, gzipTest3]

gzipTest1 :: Test
gzipTest1 =
    TestCase $ assertEqual "Test 1 of gzip3" (Just "acd") $ gzipBut3 merge continue "abc" "abd" "acc"

merge :: forall a. (Data a) => a -> a -> a -> Maybe a 
merge =
    mergeBy conflict mergeEq
    where
      conflict _a _b _c = {- trace ("\nconflict:\n a=" ++ gshow a ++ "\n b=" ++ gshow b ++ "\n c=" ++ gshow c) -} Nothing

-- Compare two values if they are of a type suitable for immediate
-- merge.  This means simple types like primitives or strings, but not
-- large data structures which the continue function would allow us to
-- traverse into.
mergeEq :: forall a. (Data a) => a -> a -> Bool
mergeEq a b = {- traceThis (\ flag -> "mergeEq -> " ++ show flag) -} (geq a b)

-- This function tells us when to abandon a merge.  At a minimum, this
-- happens when the constructors don't match.  It could also return
-- False for simple types like Strings, which we don't want to merge
-- character by character.
continue :: GB
continue x y z = {- traceThis (\ flag -> "continue -> " ++ show flag) -} (gzipQ3 x y z)

gzipTest2 :: Test
gzipTest2 =
    TestCase $ assertEqual "Test 2 of gzip3" Nothing $ gzipBut3 merge continue' "abc" "abd" "acc"

continue' :: GB
continue' =
    -- This is a magic lambda, required for typechecking
    (\ x y z -> extQ3 gzipQ3 x y z) stringFail
    where
      stringFail :: String -> String -> String -> Bool
      stringFail _ _ _ = False

gzipTest3 :: Test
gzipTest3 =
    TestCase $ assertEqual "Test 3 of gzip3" (Just "abd") $ gzipBut3 merge continue' "abc" "abd" "abc"

{-
traceThis :: (a -> String) -> a -> a
traceThis f x = trace (f x) x
-}
