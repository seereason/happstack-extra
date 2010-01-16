{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -Wwarn #-}
module Happstack.Data.IxSet.Merge
    ( threeWayMerge
    , twoOrThreeWayMerge
    -- , threeWayMerge'
    ) where

import Control.Applicative.Error
import qualified Data.ByteString as B
import Data.Data (Data, toConstr)
import Data.Generics (DataRep(AlgRep), dataTypeRep, dataTypeOf, gmapQ, extQ)
import qualified Data.Generics as G (geq, gshow)
import Data.List (intercalate)
import Data.Maybe (isJust)
--import Debug.Trace
import Happstack.Data.IxSet.Triplets (GB, GM, mkQ2, extQ2, extQ3, gzipQ3, gzipBut3, gzipBut3')

twoOrThreeWayMerge :: forall x. (Data x) => (Maybe x) -> x -> x -> Failing x
twoOrThreeWayMerge Nothing _ _ = Failure ["Unimplemented: two way merge"]
twoOrThreeWayMerge (Just o) l r = threeWayMerge o l r

-- |Untraced version, but we still trace failures via continue'.
threeWayMerge :: forall x. (Data x) => x -> x -> x -> Failing x
threeWayMerge o l r = gzipBut3 merge continue o l r

-- |If this function returns Nothing the zip will continue by trying
-- to zip the individual elements.
merge :: forall a. (Data a) => a -> a -> a -> Failing a
-- merge = mergeBy conflict eqShallow
merge o l r =
    if eqShallow o l
    then Success r
    else if eqShallow o r
         then Success l
         else if eqShallow l r
              then Success l
              else if primitive o
                   then if eqDeep l r then Success l else Failure ["Inequality: l=" ++ gshow l ++ " r=" ++ gshow r]
                   else if primitive l
                        then if eqDeep o r then Success l else Failure ["Inequality: o=" ++ gshow o ++ " r=" ++ gshow r]
                        else if primitive r
                             then if eqDeep o l then Success r else Failure ["Inequality: o=" ++ gshow o ++ " l=" ++ gshow l]
                             else Failure ["Inequality: o=" ++ gshow o ++ " l=" ++ gshow l ++ " r=" ++ gshow r]

-- This function is called when a potential conflict is detected - the
-- shallow tests have all returned false.
--conflict :: PM
--conflict _ _ _ = Nothing

-- We stop the traversal if the constructors don't
-- match, or if we encounter other values which we
-- consider primitives, such as strings.
continue :: GM
continue x y z =
    -- We need to actually pass the x y z arguments here, if
    -- we try to curry it we get a "less polymorphic" error.
    (gzipQ3 `extQ3` stringFail `extQ3` bsFail) x y z 
    where
      stringFail :: String -> String -> String -> Failing a
      stringFail a b c = Failure ["stringFail: " ++ intercalate ", " (map show [a, b, c])]
      bsFail :: B.ByteString -> B.ByteString -> B.ByteString -> Failing a
      bsFail a b c = Failure ["bsFail:" ++ intercalate ", " (map show [a, b, c])]

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

{-
threeWayMerge' :: forall x. (Data x) => x -> x -> x -> Failing x
threeWayMerge' o l r =
    traceThis (\ x -> "3 way merge " ++ failing (intercalate ", ") (const "succeeded") x) $
         gzipBut3' (\ n -> merge' n) continue' o l r

merge' :: forall a. (Data a) => Int -> a -> a -> a -> Failing a
merge' _n o l r = merge o l r

continue' :: Int -> GM
continue' _n o l r =
    case continue o l r of
      Success x -> Success x
      -- This is the important conflict message, it shows where
      -- primitive values could not be merged.
      Failure msgs -> Failure (msgs ++ ["conflict:\n o=" ++ gshow o ++ "\n l=" ++ gshow l ++ "\n r=" ++ gshow r])

traceThis :: (a -> String) -> a -> a
traceThis f x = trace (f x) x
-}

-- |Is this a primitive type?  That should depend on the application -
-- most Algebraic types are not primitives, but usually strings are,
-- and there are other types of list that can be also.  FIXME: This
-- function should really be supplied by the client.
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

--gshow' :: forall a. (Data a) => a -> String
--gshow' x = take 100 $ (gshow `extQ` bsShow) x
--    where
--      bsShow :: B.ByteString -> String
--      bsShow x = show x

gshow :: forall a. (Data a) => a -> String
gshow x = (G.gshow `extQ` bsShow) x
    where
      bsShow :: B.ByteString -> String
      bsShow x = show x

_pre :: Int -> String
_pre n = replicate n ' '

_tshow :: Data a => a -> String
_tshow x = show (dataTypeRep (dataTypeOf x))
