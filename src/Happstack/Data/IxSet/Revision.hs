{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, FunctionalDependencies,
             MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, TemplateHaskell,
             UndecidableInstances #-}
{-# OPTIONS -fno-warn-orphans -Wwarn #-}
module Happstack.Data.IxSet.Revision
    ( Ident(..)
    , Revision(..)
    , RevisionInfo(..)
    , changeRevisionInfo
    , Revisable(..)
    , NodeStatus(..)
    , copyRev
    , initialRevision
    -- , merge
    -- , revise
    , Happstack.Data.IxSet.Revision.prune
    , Heads
    , heads
    -- , combine
    --, combineInfo
    , showRev
    , combine3
    -- , combine3M
    , conflicts
    , eqEx
    ) where

--import Control.Applicative.Error
import Control.Monad (MonadPlus(mplus))
import qualified Data.ByteString.Char8 as B
import Data.Generics
import Data.List (tails, intercalate)
import qualified Data.Set as S
import qualified Data.Text as T
--import Happstack.Data (Default(..), deriveNewData, deriveNewDataNoDefault, deriveSerialize, Migrate(..))
import Happstack.Data.IxSet
import Happstack.Data.IxSet.POSet
import qualified Happstack.Data.IxSet.POSet as P
import Happstack.Data.IxSet.Triplets (mergeBy, {-mergeByM,-} mkQ2, extQ2, gzipBut3, GM)
import Happstack.State (EpochMilli)
import Happstack.Data.IxSet.Revision.Current
import Happstack.Data.IxSet.Revision.Instances()

-- |Class of values that have a revision info.
class Revisable k a | a -> k where
    getRevisionInfo :: a -> RevisionInfo k
    putRevisionInfo :: RevisionInfo k -> a -> a

copyRev :: (Revisable k a, Revisable k b) => a -> b -> b
copyRev s d = putRevisionInfo (getRevisionInfo s) d

changeRevisionInfo :: Revisable k a => (RevisionInfo k -> RevisionInfo k) -> a -> a
changeRevisionInfo f x = putRevisionInfo (f (getRevisionInfo x)) x

instance (Typeable k, Ord a, Data a, Revisable k a, Indexable a b) => POSet (IxSet a) a where
    parents s a =
        concatMap get (parentRevisions (getRevisionInfo a))
        where get n = toList (s @+ [(revision (getRevisionInfo a)) {number = n}])

-- |Initialize the revision info.
initialRevision :: Revisable k a => k -> EpochMilli -> a -> a
initialRevision newID creationTime x =
    putRevisionInfo (RevisionInfo {revision = Revision {ident = newID, number = 1},
                                   created = creationTime,
                                   parentRevisions = [],
                                   nodeStatus = Head}) x

-- |Remove all the nodes from all which are (1) in s, (2) not heads,
-- and (3) not common ancestors of heads.  This is a garbage collector
-- for a simple revision control system.  However, you can't use unless
-- you know there are no pending revisions out there waiting to happen.
prune :: forall k a. forall b. (Typeable k, Ord a, Data a, Revisable k a, Indexable a b) => IxSet a -> IxSet a -> IxSet a
prune s all =
    foldr remove (foldr reParent all reparentPairs) (S.toList victims)
    where
      (reparentPairs, victims) = 
          P.prune s (commonAncestors s (toList (s @= Head)))
      reParent (x, ps) = 
          let x' = putRevisionInfo ((getRevisionInfo x) {parentRevisions = (map (number . revision . getRevisionInfo) ps)}) x in
          insert x' . delete x
      remove x = delete x

-- |At any time a value can either have a single head revision, or it
-- may have one or more conflicting values.  Each conflicting value is
-- represented by a triple, including the nearest common ancestor
-- value and the two different head values.  (FIXME: It is possible
-- for the ancestor value to be garbage collected, so the first
-- element of the triple should be a Maybe.  In that case we need to
-- implement combine2 to try to merge this type of conflict.)
type Heads a = Maybe (Either a [(a, a, a)])

-- |Return the current value of a Revisable.
heads :: (Show k, Typeable k, Ord a, Data a, Revisable k a, Indexable a b) => IxSet a ->  Heads a
heads s =
    case toList (s @= Head) of
      [] -> Nothing
      [x] -> Just (Left x)
      xs -> Just (Right $ concatMap conflicts (tails xs))
    where
      conflicts [] = []
      conflicts (x : ys) = map (\ y -> (x, y, commonAncestor' s x y)) ys

-- |Try to do an automatic three way merge.  This traverses the data
-- structure and sees whether there are any conflicts - places where
-- both the left and right value differs from the original and from
-- each other.  If not, the combined value is returned, otherwise
-- Nothing.  Remember that the revision info will always differ, don't
-- try to merge it!
combine3 :: forall k m a. (MonadPlus m, Revisable k a) => (a -> a -> a -> m a) -> (a -> a -> Bool) -> a -> a -> a -> m a
combine3 conflict eq original left right =
    mergeBy conflict eq original (putRevisionInfo rev left) (putRevisionInfo rev right)
    where rev = getRevisionInfo original

-- | Unused?
conflicts :: forall k m a. (MonadPlus m, Revisable k a, Data a) =>
             GM -> (forall m x. (MonadPlus m, Data x) => [String] -> x -> x -> x -> m x) ->
                   (forall m x. (MonadPlus m, Data x) => x -> x -> m x) -> a -> a -> a -> m a
conflicts q conflict eq original left right =
    gzipBut3 merge q original left right
    where
      merge :: forall m x. (MonadPlus m, Data x) => x -> x -> x -> m x
      merge o l r = (eq o l >> return r) `mplus` (eq o r >> return l) `mplus` (eq l r >> return l)

-- Example implementation of the eq argument to combine3.
eqEx :: GenericQ (GenericQ Bool)
eqEx x y =
    (geq `mkQ2` bsEq `extQ2` stringEq `extQ2` textEq) x y
    where
      -- If the specialized eqs don't work, use the generic.  This
      -- will throw an exception if it encounters something with a
      -- NoRep type.
      geq :: (Data a, Data b) => a -> b -> Bool
      geq x y = (toConstr x == toConstr y) && and (gzipWithQ eqEx x y)
      stringEq :: String -> String -> Bool
      stringEq a b = (a == b)
      textEq :: T.Text -> T.Text -> Bool
      textEq a b = (a == b)
      bsEq :: B.ByteString -> B.ByteString -> Bool
      bsEq a b = (a == b)

showRev :: Show k => RevisionInfo k -> String
showRev r = (show . ident . revision $ r) ++ "." ++ (show . number . revision $ r) ++ " " ++ show (parentRevisions r)

-- |A version of common ancestor that assumes there is one.
commonAncestor' :: (Show k, Revisable k a, Data a, POSet (IxSet a) a) => IxSet a -> a -> a -> a
commonAncestor' s x y = maybe (error $ message s x y) id (commonAncestor s x y)

message :: (Show k, Revisable k a, Data a, POSet (IxSet a) a) => IxSet a -> a -> a -> String
message s x y =
    ("No common ancestor: " ++ showRev rx ++ ", " ++ showRev ry ++
     "\n  parents " ++ showRev rx ++ " -> " ++ intercalate " " (map (showRev . getRevisionInfo) (parents s x))  ++
     "\n  parents " ++ showRev ry ++ " -> " ++ intercalate " " (map (showRev . getRevisionInfo) (parents s y))  ++
     "\n  s = [" ++ intercalate " " (map (showRev . getRevisionInfo) (toList s)) ++ "]" ++
     "\n  commonAncestors -> " ++ intercalate " " (map (showRev . getRevisionInfo) (S.toList (commonAncestors s [x, y]))))
    where rx = getRevisionInfo x
          ry = getRevisionInfo y
