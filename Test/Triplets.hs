{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}
module Test.Triplets
    ( tests
    ) where

import Control.Applicative.Error
import Control.Monad (MonadPlus(..))
import Happstack.Data.IxSet.Triplets
import Data.Generics (Data, geq, gshow)
import Data.List (intercalate)
import Test.HUnit

tests :: [Test]
tests = [gzipTest1, gzipTest2, gzipTest3]

gzipTest1 :: Test
gzipTest1 =
    TestCase $ assertEqual "Test 1 of gzip3" (Success "acd") $ gzipBut3 merge continue "abc" "abd" "acc"

merge :: forall m a. (MonadPlus m, Data a) => a -> a -> a -> m a 
merge =
    mergeBy conflict mergeEq
    where
      conflict :: a -> a -> a -> m a
      conflict a b c = fail ("Test.Triplets.merge: " ++ intercalate ", " (map gshow [a, b, c]))

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
continue :: GM
continue x y z = {- traceThis (\ flag -> "continue -> " ++ show flag) -} (gzipQ3 x y z)

gzipTest2 :: Test
gzipTest2 =
    TestCase $ assertEqual "Test 2 of gzip3" (Failure ["Test.Triplets.merge: \"abc\", \"abd\", \"acc\"","Test.Triplets.stringFail: \"abc\", \"abd\", \"acc\""]) $ gzipBut3 merge continue' "abc" "abd" "acc"

continue' :: GM
continue' =
    -- This is a magic lambda, required for typechecking
    (\ x y z -> extQ3 gzipQ3 x y z) stringFail

stringFail :: Monad m => String -> String -> String -> m a
stringFail a b c = fail ("Test.Triplets.stringFail: " ++ intercalate ", " (map show [a, b, c]))

gzipTest3 :: Test
gzipTest3 =
    TestCase $ assertEqual "Test 3 of gzip3" (Success "abd") $ gzipBut3 merge continue' "abc" "abd" "abc"
