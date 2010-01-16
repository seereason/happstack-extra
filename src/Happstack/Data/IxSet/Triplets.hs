{-# LANGUAGE RankNTypes #-}
{-# OPTIONS -Wwarn #-}
module Happstack.Data.IxSet.Triplets
    ( gzipWithM3
    , gzipWithT3
    , gzip3
    , gzipQ3
    , gzipBut3
    , gzipBut3'
    , extQ2
    , extQ3
    , extT3
    , mkQ3
    , mkQ2
    -- , mkP3
    -- , gzip3F
    , mergeBy
    , mergeByM
    , GB, GM, PB, PM
    ) where

import Prelude hiding (GT)
import Control.Applicative.Error
import Data.Generics (Data, Typeable, toConstr, cast, gcast, gmapAccumQ, orElse, gshow,
                      unGT, GenericT, GenericT'(GT), gmapAccumT,
                      unGM, GenericM, GenericM'(GM), gmapAccumM,
                      unGQ, GenericQ, GenericQ'(GQ), gmapQ)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)

import Debug.Trace

cast' :: (Typeable a, Typeable b) => a -> Failing b
cast' = maybe (Failure ["cast"]) Success . cast

orElse' :: Failing a -> Failing a -> Failing a
orElse' a b =
    case a of
      Success x -> Success x
      Failure ma ->
          case b of
            Success x -> Success x
            Failure mb -> Failure (ma ++ mb)

-- As originally defined: Twin map for transformation

{-
gzipWithT2 :: GenericQ (GenericT) -> GenericQ (GenericT)
gzipWithT2 f x y = case gmapAccumT perkid funs y of
                   ([], c) -> c
                   _       -> error "gzipWithT2"
 where
 perkid a d = (tail a, unGT (head a) d)
 funs = gmapQ (\k -> GT (f k)) x
-}


-- As originally defined: Twin map for transformation

{-
gzipWithM2 :: Monad m => GenericQ (GenericM m) -> GenericQ (GenericM m)
gzipWithM2 f x y = case gmapAccumM perkid funs y of
                   ([], c) -> c
                   _       -> error "gzipWithM"
 where
 perkid a d = (tail a, unGM (head a) d)
 funs = gmapQ (\k -> GM (f k)) x
-}


-- As originally defined: generic zip

{-
gzip2 ::
   (forall x. Data x => x -> x -> Maybe x)
 -> (forall x. Data x => x -> x -> Maybe x)

gzip2 f = gzip2' f'
 where
 f' :: GenericQ (GenericM Maybe)
 f' x y = cast x >>= \x' -> f x' y
 gzip2' :: GenericQ (GenericM Maybe) -> GenericQ (GenericM Maybe)
 gzip2' f x y =
   f x y
   `orElse`
   if toConstr x == toConstr y
     then gzipWithM2 (gzip2' f) x y
     else Nothing
-}

-- For three args now

gzipWithT3 ::
   GenericQ (GenericQ (GenericT))
 -> GenericQ (GenericQ (GenericT))
gzipWithT3 f x y z =
   case gmapAccumT perkid funs z of
     ([], c) -> c
     _       -> error "gzipWithT3"
 where
 perkid a d = (tail a, unGT (head a) d)
 funs = case gmapAccumQ perkid' funs' y of
          ([], q) -> q
          _       -> error "gzipWithT3"
  where
   perkid' a d = (tail a, unGQ (head a) d)
   funs' = gmapQ (\k -> (GQ (\k' -> GT (f k k')))) x

gzipWithM3 :: Monad m
 => GenericQ (GenericQ (GenericM m))
 -> GenericQ (GenericQ (GenericM m))
gzipWithM3 f x y z =
    case gmapAccumM perkid funs z of
      ([], c) -> c
      _       -> error "gzipWithM3"
    where
      perkid a d = (tail a, unGM (head a) d)
      funs = case gmapAccumQ perkid' funs' y of
               ([], q) -> q
               _       -> error "gzipWithM3"
          where
            perkid' a d = (tail a, unGQ (head a) d)
            funs' = gmapQ (\k -> (GQ (\k' -> GM (f k k')))) x

type GB = GenericQ (GenericQ (GenericQ Bool))                   -- Generic Bool Query
--      = (Data a, Data b, Data c) => a -> b -> c -> Bool
type GM = GenericQ (GenericQ (GenericM Failing))                -- Generic Maybe Query
--      = (Data a, Data b, Data c) => a -> b -> c -> Maybe c
type PB = forall x. Data x => x -> x -> x -> Bool
type PM = forall x. Data x => x -> x -> x -> Failing x        -- Polymorphic Failing Query

-- |The purpose of gzip3 is to map a polymorphic (generic) function
-- over the "elements" of three instances of a type.  The function
-- returns a Maybe value of the same type as the elements passed.  If
-- it returns a Just the subtree is not traversed, the returned value
-- is used.  If it returns Nothing the subtree is traversed.  This
-- traversal may succeed where the top level test failed, resulting in
-- a successful zip.  For example, the merge function wouldn't merge
-- these three values:
--     (1, 1)  (1, 2) (2, 1) -> (?, ?)
-- but it could merge the two unzipped triples:
--     (1, 1, 2) -> 2
--     (1, 2, 1) -> 2
--       -> (2, 2)
gzip3 :: PM -> PM
gzip3 f = gzipBut3 f gzipQ3

-- |This is the minimal condition for recursing into a value - the
-- constructors must all match.
gzipQ3 :: GM
gzipQ3 x y z = 
    if and [toConstr x == toConstr y, toConstr y == toConstr z]
    then Success undefined
    else Failure ["Conflict: x=" ++ gshow x ++ " y=" ++ gshow y ++ " z=" ++ gshow z]

-- |This function adds a test to limit the recursion of gzip3.  For
-- example, with the merge function mentioned above you might want to
-- avoid merging strings character by character:
-- 
--     gzip3 merge "dim" "kim" "dip" -> Just "kip" (no!)
-- 
-- so you would pass a limiting function to prevent recursing into strings:
-- 
--     let continue =
--          (\ x y z -> extQ3 gzipQ3 x y z) x y z
--          where
--            stringFail :: String -> String -> String -> Bool
--            stringFail _ _ _ = False
--     gzipBut3 merge continue "dim" "kim" "dip" -> Nothing
-- 
-- this can also save a lot of time examining all the heads and tails
-- of every string.
gzipBut3 :: PM -> GM -> PM
gzipBut3 merge continue x y z =
    gzip3' merge' x y z
    where
      -- If the three elements aren't all the type of f's arguments,
      -- this expression will return Nothing.  Also, the f function
      -- might return Nothing.  In those cases we call gzipWithM3 to
      -- traverse the sub-elements.
      merge' :: GM
      merge' x y z = cast' x >>= \x' -> cast' y >>= \y' -> merge x' y' z
      gzip3' :: GM -> GM
      gzip3' merge x y z =
          merge x y z
         `orElse'`
           case continue x y z of
             Success _ -> gzipWithM3 (gzip3' merge) x y z
             Failure msgs -> Failure msgs

gzipBut3' :: (Int -> PM) -> (Int -> GM) -> PM
gzipBut3' merge continue x y z =
    gzip3' 0 merge' x y z
    where
      -- If the three elements aren't all the type of f's arguments,
      -- this expression will return Nothing.  Also, the f function
      -- might return Nothing.  In those cases we call gzipWithM3 to
      -- traverse the sub-elements.
      merge' :: Int -> GM
      merge' n x y z = cast' x >>= \x' -> cast' y >>= \y' -> merge n x' y' z
      gzip3' :: Int -> (Int -> GM) -> GM
      gzip3' n merge x y z =
          merge n x y z
         `orElse'`
           case continue n x y z of
             Success _ -> gzipWithM3 (gzip3' (n+1) merge) x y z
             Failure msgs -> Failure msgs

extQ2 :: (Typeable a, Typeable b, Typeable d, Typeable e)
      => (a -> b -> r) -> (d -> e -> r) -> a -> b -> r
extQ2 d q x y = fromMaybe (d x y) $ cast x >>= \x' -> cast y >>= \y' -> Just (q x' y')

extQ3 :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f)
      => (a -> b -> c -> r) -> (d -> e -> f -> r) -> a -> b -> c -> r
extQ3 d q x y z = fromMaybe (d x y z) $ cast x >>= \x' -> cast y >>= \y' -> cast z >>= \z' -> Just (q x' y' z')

mkQ3 :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f)
     => r -> (a -> b -> c -> r) -> d -> e -> f -> r
mkQ3 d q x y z = extQ3 (\ _ _ _ -> d) q x y z

extT3 :: (Typeable a, Typeable b)
      => (a -> a -> a -> Maybe a) -> (b -> b -> b -> Maybe b) -> a -> a -> a -> Maybe a
extT3 d q x y z = fromMaybe (d x y z) $ cast x >>= \x' -> cast y >>= \y' -> cast z >>= \z' -> gcast (q x' y' z')

mkQ2 :: (Data a, Data b, Data c) => (a -> b -> r) -> (c -> c -> r) -> a -> b -> r
mkQ2 d q x y = fromMaybe (d x y) $ cast x >>= \x' -> cast y >>= \y' -> Just (q x' y')

-- |This function implements the f function used to do three way
-- merging.  A triplet (original, new1, new2) conflicts if the two
-- new values each differ from the original, and from each other.
-- Otherwise, the new value that differs from the original is kept, or
-- either of the new values if they match.  However, even if the
-- values conflict, it still might be possible to do the merge by
-- examining the components of the value.  So conflict is typically
-- (\ _ _ _ -> Nothing), while eq could be geq, but it could also
-- just return false for more complex datatypes that we don't want
-- to repeatedly traverse.
mergeBy :: forall a. (a -> a -> a -> Failing a) -> (a -> a -> Bool) -> a -> a -> a -> Failing a
mergeBy conflict eq original left right =
    if eq original left then Success right
    else if eq original right then Success left
         else if eq left right then Success left
              else conflict original left right

mergeByM :: forall a m. (Monad m) => (a -> a -> a -> m (Failing a)) -> (a -> a -> Bool) -> a -> a -> a -> m (Failing a)
mergeByM conflict eq original left right =
    if eq original left then return (Success right)
    else if eq original right then return (Success left)
         else if eq left right then return (Success left)
              else conflict original left right

_traceThis :: (a -> String) -> a -> a
_traceThis f x = trace (f x) x

{-
mkP3 :: forall a b c r. (Data a, Data b, Data c) => r -> (a -> a -> a -> r) -> b -> c -> a -> r
mkP3 d f x y z = fromMaybe d $ cast x >>= \x' -> cast y >>= \y' -> Just (f x' y' z)
-}

-- traceF x = trace ("f -> " ++ if isJust x then "Just ..." else "Nothing") x
-- traceQ x = trace ("q -> " ++ show x) x
-- traceZip x = trace ("gzipWithM3 -> " ++ if isJust x then "Just ..." else "Nothing") x
