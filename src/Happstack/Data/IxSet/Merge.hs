{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -Wwarn #-}
module Happstack.Data.IxSet.Merge
    ( threeWayMerge
    , threeWayMerge'
    ) where

import qualified Data.ByteString as B
import Data.Data (Data, toConstr)
import Data.Generics (DataRep(AlgRep), dataTypeRep, dataTypeOf, gmapQ, extQ)
import qualified Data.Generics as G
import Data.List (replicate)
import Data.Maybe (isJust)
import Debug.Trace
import Happstack.Data.IxSet.Triplets (GB, PM, mkQ2, extQ2, extQ3, gzipQ3, gzipBut3, gzipBut3')

threeWayMerge :: forall x. (Data x) => x -> x -> x -> Maybe x
threeWayMerge o l r = gzipBut3 merge (continue' 0) o l r

-- |If this function returns Nothing the zip will continue by trying
-- to zip the individual elements.
merge :: forall a. (Data a) => a -> a -> a -> Maybe a
-- merge = mergeBy conflict eqShallow
merge o l r =
    if eqShallow o l
    then Just r
    else if eqShallow o r
         then Just l
         else if eqShallow l r
              then Just l
              else if primitive o
                   then if eqDeep l r then Just l else Nothing
                   else if primitive l
                        then if eqDeep o r then Just l else Nothing
                        else if primitive r
                             then if eqDeep o l then Just r else Nothing
                             else Nothing

-- This function is called when a potential conflict is detected - the
-- shallow tests have all returned false.
conflict :: PM
conflict _ _ _ = Nothing

-- We stop the traversal if the constructors don't
-- match, or if we encounter other values which we
-- consider primitives, such as strings.
continue :: GB
continue x y z =
    -- We need to actually pass the x y z arguments here, if
    -- we try to curry it we get a "less polymorphic" error.
    (gzipQ3 `extQ3` stringFail `extQ3` bsFail) x y z 
    where
      stringFail :: String -> String -> String -> Bool
      stringFail _ _ _ = False
      bsFail :: B.ByteString -> B.ByteString -> B.ByteString -> Bool
      bsFail _ _ _ = False

-- |Shallow equalify function.  This will return False for records
-- whose fields might differ.  If we simply compared the constructors
-- here it would seem that all records with the same constructor are
-- equal, so we need to return false if the record has fields which
-- might or might not be equal.
eqDeep :: forall a. (Data a) => a -> a -> Bool
eqDeep a b = (G.geq `mkQ2` stringEq `extQ2` bsEq) a b

eqShallow :: forall a. (Data a) => a -> a -> Bool
eqShallow a b =
    (eq `mkQ2` stringEq `extQ2` bsEq) a b
    where
      eq :: forall a. (Data a) => a -> a -> Bool
      eq a b = case dataTypeRep (dataTypeOf a) of
                 AlgRep _ | length (gmapQ (const ()) a) > 0 -> False
                 _ -> toConstr a == toConstr b

stringEq :: String -> String -> Bool
stringEq = (==)

bsEq :: B.ByteString -> B.ByteString -> Bool
bsEq = (==)

-- Traced version

threeWayMerge' :: forall x. (Data x) => x -> x -> x -> Maybe x
threeWayMerge' o l r =
    traceThis (\ x -> "3 way merge " ++ if isJust x then "succeeded" else "failed") $
         gzipBut3' (\ n -> merge' n) continue' o l r

merge' :: forall a. (Data a) => Int -> a -> a -> a -> Maybe a
merge' _n o l r =
    maybe ({- trace (pre n ++ "merge failed: merge " ++ gshow o ++ " " ++ gshow l ++ " " ++ gshow r ++ " -> Nothing") -} Nothing)
          (\ merged -> {- trace (pre n ++ "merge succeeded: merge " ++ gshow o ++ " " ++ gshow l ++ " " ++ gshow r ++ " -> Just " ++ gshow merged) -} (Just merged))
          (merge o l r)

continue' :: Int -> GB
continue' _n o l r =
    case continue o l r of
      True -> True
      False -> trace ("conflict:\n o=" ++ gshow o ++ "\n l=" ++ gshow l ++ "\n r=" ++ gshow r) False

traceThis :: (a -> String) -> a -> a
traceThis f x = trace (f x) x

primitive :: Data a => a -> Bool
primitive x =
    (prim `extQ` isBS `extQ` isString) x
    where
      isBS :: B.ByteString -> Bool
      isBS _ = True
      isString :: String -> Bool
      isString _ = True

prim :: Data a => a -> Bool
prim x = 
    case dataTypeRep (dataTypeOf x) of
      AlgRep _ | length (gmapQ (const ()) x) > 0 -> False
      _ -> True

gshow' :: forall a. (Data a) => a -> String
gshow' x = take 100 $ (G.gshow `extQ` bsShow) x
    where
      bsShow :: B.ByteString -> String
      bsShow x = show x

gshow :: forall a. (Data a) => a -> String
gshow x = (G.gshow `extQ` bsShow) x
    where
      bsShow :: B.ByteString -> String
      bsShow x = show x

pre :: Int -> String
pre n = replicate n ' '

_tshow :: Data a => a -> String
_tshow x = show (dataTypeRep (dataTypeOf x))
