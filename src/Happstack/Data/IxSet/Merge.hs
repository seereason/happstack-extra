{-# LANGUAGE ScopedTypeVariables, Rank2Types #-}
{-# OPTIONS -Wwarn #-}
module Happstack.Data.IxSet.Merge
    ( threeWayMerge
    , twoOrThreeWayMerge
    , continue
    , threeWayMergeA
    , twoOrThreeWayMergeA
    , continueA
    ) where

import Control.Applicative (Applicative(..))
import Control.Monad (MonadPlus(..))
import qualified Data.ByteString as B
import Data.Data (Data, toConstr)
import Data.Generics (DataRep(AlgRep), dataTypeRep, dataTypeOf, gmapQ, extQ, gshow)
import qualified Data.Generics as G (geq)
import Data.Text (Text)
import Happstack.Data.IxSet.Triplets (GM, GA, GB, PM, PA, mkQ2, extQ2, extQ3, extT3, gzipQ3, gzipBut3, gzipButA3)

twoOrThreeWayMerge :: forall m x. (MonadPlus m, Data x) => GM -> (Maybe x) -> x -> x -> m x
twoOrThreeWayMerge _ Nothing _ _ = fail "Unimplemented: two way merge"
twoOrThreeWayMerge continue (Just o) l r = threeWayMerge continue o l r

-- |Untraced version, but we still trace failures via continue'.
threeWayMerge :: GM -> PM
threeWayMerge continue o l r = gzipBut3 merge continue o l r

twoOrThreeWayMergeA :: forall f x. (Applicative f, Data x) => GB -> GA f -> (Maybe x) -> x -> x -> f x
twoOrThreeWayMergeA _ _ Nothing _ _ = error "Unimplemented: two way merge"
twoOrThreeWayMergeA continue conflict (Just o) l r = threeWayMergeA continue conflict o l r

-- |Untraced version, but we still trace failures via continue'.
threeWayMergeA :: forall f. (Applicative f) => GB -> PA f -> PA f
threeWayMergeA continue conflict o l r = gzipButA3 merge continue conflict o l r

-- |If this function returns Nothing the zip will continue by trying
-- to zip the individual elements.
-- 
-- It seems like all these deep equality calls are wasteful.  Not sure
-- how to avoid this.
merge :: PM
merge o l r =
    if eqShallow o l
    then return r
    else if eqShallow o r
         then return l
         else if eqShallow l r
              then return l
              else if primitive o
                   then if eqDeep l r then return l else mzero
                   else if primitive l
                        then if eqDeep o r then return l else mzero
                        else if primitive r
                             then if eqDeep o l then return r else mzero
                             else mzero

-- |This function is called by Triplets.gzipBut3 after the
-- straightforward merge fails.  This happens when we encountere
-- unequal primitive values or we can't find a three way merge using
-- deep equality.  In this case, continue is called to decide whether
-- to keep traversing the values.  If the arguments are Strings or
-- ByteStrings the stringFail or bsFail functions will return a
-- Failure, indicating there was a conflict.  Otherwise gzipQ3 will
-- see if the constructors match, in which case we will continue to
-- try to merge the individual fields of the three values.
--
-- This could be extended to prevent traversal of other types we want
-- to consider primitive, which is why it is passed to
-- twoOrThreeWayMerge rather than being called from there directly.
continue :: GM
continue o l r =
    -- We need to actually pass the three arguments here, if
    -- we try to curry it we get a "less polymorphic" error.
    (gzipQ3 `extQ3` stringFail `extQ3` bsFail) o l r

continueA :: GB
continueA o l r =
    (constrMatch `extQ3` stringFail `extQ3` bsFail `extQ3` textFail) o l r
    where
      constrMatch :: GB
      constrMatch o l r = and [toConstr o == toConstr l, toConstr o == toConstr r]
      stringFail :: String -> String -> String -> Bool
      stringFail _ _ _ = {- trace "stringFail" -} False
      bsFail :: B.ByteString -> B.ByteString -> B.ByteString -> Bool
      bsFail _ _ _ = {- trace "bsFail" -} False
      textFail :: Text -> Text -> Text -> Bool
      textFail _ _ _ = {- trace "textFail" -} False

stringFail :: forall m a. MonadPlus m => String -> String -> String -> m a
stringFail o l r = fail ("String conflict: o=" ++ show o ++ ", l=" ++ show l ++ ", r=" ++ show r)

bsFail :: forall m a. MonadPlus m => B.ByteString -> B.ByteString -> B.ByteString -> m a
bsFail o l r = fail ("Bytestring conflict: o=" ++ show o ++ ", l=" ++ show l ++ ", r=" ++ show r)

textFail :: forall m a. MonadPlus m => Text -> Text -> Text -> m a
textFail o l r = fail ("Text conflict: o=" ++ show o ++ ", l=" ++ show l ++ ", r=" ++ show r)

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

_pre :: Int -> String
_pre n = replicate n ' '

_tshow :: Data a => a -> String
_tshow x = show (dataTypeRep (dataTypeOf x))
