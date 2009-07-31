{-# LANGUAGE RankNTypes #-}

module Happstack.Data.IxSet.Triplets
    ( gzipWithM3
    , gzipWithT3
    , gzip3
    -- , combine3
    ) where

import Prelude hiding (GT)
import Data.Generics

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

gzip3 ::
   (forall x. Data x => x -> x -> x -> Maybe x)
 -> (forall x. Data x => x -> x -> x -> Maybe x)

gzip3 f = gzip3' f'
 where
 f' :: GenericQ (GenericQ (GenericM Maybe))
 f' x y z = cast x >>= \x' -> cast y >>= \y' -> f x' y' z
 gzip3' ::
      GenericQ (GenericQ (GenericM Maybe))
   -> GenericQ (GenericQ (GenericM Maybe))
 gzip3' f x y z =
   f x y z
   `orElse`
   if and [toConstr x == toConstr y, toConstr y == toConstr z]
     then gzipWithM3 (gzip3' f) x y z
     else Nothing
