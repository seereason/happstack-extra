{-# LANGUAGE ScopedTypeVariables #-}
module Happstack.Data.IxSet.Merge
    ( threeWayMerge
    , threeWayMerge'
    , mergeEq'
    , shallowEq'
    , stringEq'
    , bsEq'
    ) where

import qualified Data.ByteString as B
import Data.Data (Data, toConstr)
import Data.Generics (gshow, geq, DataRep(AlgRep), dataTypeRep, dataTypeOf, gmapQ)
import Data.Maybe (isJust)
import Debug.Trace
import Happstack.Data.IxSet.Triplets (GB, PM, mkQ2, extQ2, extQ3, extT3, gzipQ3, mergeBy, gzipBut3)
import Happstack.Data.IxSet.Revision (NodeStatus)

threeWayMerge :: forall x. (Data x) => x -> x -> x -> Maybe x
threeWayMerge o l r = gzipBut3 merge continue o l r
    -- traceThis (\ x -> "3 way merge " ++ if isJust x then "succeeded" else "failed") $ gzipBut3 merge continue o l r

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

-- This function is called when a conflict is detected
conflict :: PM
conflict _ _ _ = Nothing

-- This function is used to decide whether elements are
-- equal.  However, the comparison is only final when this
-- returns True - if it returns False the zip continues.
-- Therefore, if we detect a complex datatype that we don't
-- want to traverse with geq, we just return False.

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
mergeEq :: forall a. (Data a) => a -> a -> Bool
mergeEq a b = (shallowEq `mkQ2` stringEq `extQ2` bsEq) a b

shallowEq :: forall a. (Data a) => a -> a -> Bool
shallowEq a b =
    case dataTypeRep (dataTypeOf a) of
      AlgRep _ | length (gmapQ (const ()) a) > 0 -> False
      -- NoRep -> error $ "Can't compare " ++ gshow a ++ " " ++ gshow b
      _ -> toConstr a == toConstr b

stringEq :: String -> String -> Bool
stringEq = (==)

bsEq :: B.ByteString -> B.ByteString -> Bool
bsEq = (==)

-- |This works, but it does a lot of (I believe) unnecessary computation.
_mergeEqDeep :: forall a. (Data a) => a -> a -> Bool
_mergeEqDeep a b =
    (eq `mkQ2` stringEq `extQ2` nodeStatusEq) a b
    where
      -- Using the real geq here will cause lots of computation
      -- when we encounter large structures which appear to
      -- conflict somewhere.
      eq :: a -> a -> Bool
      eq a b = {- traceThis (\ flag -> "geq a b -> " ++ show flag) $ -} geq a b
      -- If we use this we get 
      --- threeWayMerge (1, "unedited", 2, "unedited") (1, "hello", 2, "unedited") (1, "unedited", 2, "world") ->
      --     Just (1,"unedited",2,"world")
      -- geq' a b = traceThis (\ flag -> "geq a b -> " ++ show flag) (toConstr a == toConstr b)
      stringEq :: String -> String -> Bool
      stringEq a b = {- trace (show a ++ " == " ++ show b ++ " -> " ++ show (a == b)) -} (a == b)
      -- This is just here for diagnostic purposes
      nodeStatusEq :: NodeStatus -> NodeStatus -> Bool
      nodeStatusEq a b = {- trace ("nodeStatusEq " ++ show a ++ " " ++ show b ++ " -> " ++ show (a == b)) -} (a == b)

-- Traced version

threeWayMerge' :: forall x. (Data x) => x -> x -> x -> Maybe x
threeWayMerge' o l r =
    traceThis (\ x -> "3 way merge " ++ if isJust x then "succeeded" else "failed") $ gzipBut3 merge' continue o l r

merge' :: forall a. (Data a) => a -> a -> a -> Maybe a
-- merge' = mergeBy conflict' mergeEq'
merge' = mergeBy conflict' mergeEq

conflict' :: PM
conflict' o l r =
    (gConflict `extT3` bsConflict) o l r
    where
      gConflict :: PM
      gConflict o l r =
          -- This will make it slow again
          if geq o l && geq o r
          then Nothing
          else trace ("conflict:\n o=" ++ gshow o ++ "\n l=" ++ gshow l ++ "\n r=" ++ gshow r) Nothing
      bsConflict :: B.ByteString -> B.ByteString -> B.ByteString -> Maybe B.ByteString
      bsConflict o l r = trace ("bsConflict:\n o=" ++ show o ++ "\n l=" ++ show l ++ "\n r=" ++ show r) Nothing

mergeEq' :: forall a. (Data a) => a -> a -> Bool
mergeEq' a b = (shallowEq' `mkQ2` stringEq' `mkQ2` bsEq') a b

shallowEq' :: forall a. (Data a) => a -> a -> Bool
shallowEq' a b = traceThis (\ flag -> "shallowEq " ++ gshow a ++ " " ++ gshow b ++ " -> " ++ show flag) $ shallowEq a b

stringEq' :: String -> String -> Bool
stringEq' a b = traceThis (\ flag -> "stringEq " ++ show a ++ " " ++ show b ++ " -> " ++ show flag) $ stringEq a b

bsEq' :: B.ByteString -> B.ByteString -> Bool
bsEq' a b = traceThis (\ flag -> "bsEq " ++ show a ++ " " ++ show b ++ " -> " ++ show flag) $ bsEq a b

traceThis :: (a -> String) -> a -> a
traceThis f x = trace (f x) x

_tshow :: Data a => a -> String
_tshow x = show (dataTypeRep (dataTypeOf x))
