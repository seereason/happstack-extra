{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables #-}
module Test.Triplets
    ( tests
    ) where

import Happstack.Data.IxSet.Triplets
import Data.Generics (Data, geq, gshow, toConstr)
import Test.HUnit
import Debug.Trace

tests :: [Test]
tests = [gzipTest1]

gzipTest1 =
    TestCase $ assertEqual "Test 1 of gzip3" (Just "acd") $ zip "abc" "abd" "acc"
    where
      zip :: forall a. (Data a) => a -> a -> a -> Maybe a
      zip = gzip3But continue merge

merge :: forall a. (Data a) => a -> a -> a -> Maybe a 
merge =
    mergeBy conflict mergeEq
    where
      conflict a b c = trace ("\nconflict:\n a=" ++ gshow a ++ "\n b=" ++ gshow b ++ "\n c=" ++ gshow c) Nothing

-- Compare two values if they are of a type suitable for immediate
-- merge.  This means simple types like primitives or strings, but not
-- large data structures which the continue function would allow us to
-- traverse into.
mergeEq :: forall a. (Data a) => a -> a -> Bool
mergeEq a b = traceThis (\ flag -> "mergeEq -> " ++ show flag) (geq a b)

-- This function tells us when to abandon a merge.  At a minimum, this
-- happens when the constructors don't match.  It could also return
-- False for simple types like Strings, which we don't want to merge
-- character by character.
continue :: GB
continue x y z = traceThis (\ flag -> "continue -> " ++ show flag) (gzip3Q x y z)

{-
gzipTest2 =
    TestCase $ assertEqual "Test 2 of gzip3" Nothing $ zip "abc" "abd" "acc"
    where
      zip :: forall a. (Data a) => a -> a -> a -> Maybe a
      zip = gzip3But continue' merge

continue' :: GB
continue' =
    extQ3 gzip3Q stringFail
    where
      stringFail :: String -> String -> String -> Bool
      stringFail _ _ _ = False
-}

traceThis :: (a -> String) -> a -> a
traceThis f x = trace (f x) x
