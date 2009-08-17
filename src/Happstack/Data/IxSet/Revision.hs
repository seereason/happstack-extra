{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances,
             MultiParamTypeClasses, RankNTypes, TemplateHaskell,
             UndecidableInstances #-}
{-# OPTIONS -fno-warn-orphans #-}
module Happstack.Data.IxSet.Revision
    ( Ident(..)
    , Revision(..)
    , RevisionInfo(..)
    , Revisable(..)
    , initialRevision
    , merge
    , revise
    , Happstack.Data.IxSet.Revision.prune
    , Heads
    , heads
    , combine
    , combineInfo
    , showRev
    , combine3
    , combine3traced
    , eqEx
    ) where

--import Control.Applicative.Error (Failing(..))
import qualified Data.ByteString.Char8 as B
import Data.Generics
import Data.List (tails, partition, intercalate)
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as S
import Happstack.Data (Default(..), deriveNewData, deriveSerialize)
import Happstack.Data.IxSet
import Happstack.State (Version(..))
--import Text.Formlets (xml, check)
--import Text.Formlets.Generics.Instances ()
--import Text.Formlets.Generics.Markup.Types (Markup(..))
import Happstack.Data.IxSet.POSet
import qualified Happstack.Data.IxSet.POSet as P
import Happstack.Data.IxSet.Triplets (mergeBy, mergeByTraced, mkQ2, extQ2)

import Debug.Trace

-- We need newtypes for each of these so we can make them IxSet
-- indexes.  That is also why they must each be a separate field
-- of the revisioned type.

-- | Identifier for a item which can have multiple revisions.
newtype Ident
    = Ident {unIdent :: Integer}
    deriving (Eq, Ord, Read, Show, Data, Typeable)

-- | Identifier for a particular revision of a particular item.
data Revision
    = Revision {ident :: Ident, number :: Integer}
    deriving (Eq, Ord, Read, Show, Data, Typeable)

-- | The information associated with a revision to record its status.
data RevisionInfo
    = RevisionInfo {revision :: Revision, parentRevisions :: [Integer], isHead :: Bool}
    deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveNewData [''Ident])
instance Version Ident
$(deriveSerialize ''Ident)
$(deriveNewData [''Revision])
instance Version Revision
$(deriveSerialize ''Revision)
$(deriveNewData [''RevisionInfo])
instance Version RevisionInfo
$(deriveSerialize ''RevisionInfo)

-- |Class of values that have a revision info.
class Revisable a where
    getRevisionInfo :: a -> RevisionInfo
    putRevisionInfo :: RevisionInfo -> a -> a

instance (Ord a, Data a, Revisable a, Indexable a b) => POSet (IxSet a) a where
    parents s a =
        concatMap get (parentRevisions (getRevisionInfo a))
        where get n = toList (s @+ [(revision (getRevisionInfo a)) {number = n}])

{-
-- |A Set s of Revisable a's.
class Revisable a => RevisableSet s a where
    -- |Return the newest a
    newest :: s -> a
    -- |Return all the heads, the leaves of the revision tree.
    heads :: s -> [a]
    -- |Assign a revision number and insert into s
    insert :: a -> s -> s
    -- |The newest common ancestor
    ancestor :: a -> a -> a
-}

-- |Initialize the revision info.
initialRevision :: Revisable a => Ident -> a -> a
initialRevision newID x =
    putRevisionInfo (RevisionInfo {revision = Revision {ident = newID, number = 1},
                                   parentRevisions = [],
                                   isHead = True}) x

-- |Insert a revision into the index and designate it the merger of a
-- list of existing revisions.  This first checks that all the parents
-- are heads - you can't merge something that has already been merged
-- or revised, though you can create a branch using revise.
merge :: (Ord a, Data a, Revisable a, Indexable a b) => IxSet a -> IxSet a -> [a] -> a -> (IxSet a, a)
merge s all parents merged =
    if any (not . isHead . getRevisionInfo) parents
    then error "Attempt to merge non-head revision"
    else let (all', _, merged') = merge' all s parents merged in (all', merged')

-- |Revise is a special case of merge, where we replace a single
-- element (rather than several) with one new element.
revise :: (Revisable a, Ord a, Data a, Indexable a b) => IxSet a -> IxSet a -> a -> (IxSet a, a)
revise all revs revised =
    -- If s is empty there are no previous revisions, so we need to
    -- create revision 1.
    (all', revised')
    where (all', _, revised') =
              merge' all revs (toList (all @= revision (getRevisionInfo revised))) revised

-- |Helper function for creating, merging and revising.  It inserts a
-- revision into the index and designates it the merger of zero or
-- more revisions given by the argument.  It returns a pair containing
-- the new set and the new element.
merge' :: forall a. forall b. (Ord a, Data a, Revisable a, Indexable a b) =>
          IxSet a -> IxSet a -> [a] -> a -> (IxSet a, IxSet a, a)
merge' all s parents merged =
    (all', s', merged')
    where
      -- Add the new element to all, and reset the isHead flag of all the parents
      all' = insert merged' all''
      all'' = foldr (\ x -> insert (unHead x) . del (revision (getRevisionInfo x))) all parents
      -- Add the new element to the head set, and remove all the parents
      s' = insert merged' (foldr delete s parents)
      -- Put the new revision info into the merged element
      merged' = putRevisionInfo info' merged
      -- Create the new revision info
      info' =
          if any ((/=) . i $ merged) (map i parents)
          then error $ "merge': ident mismatch: " ++ show (i merged) ++ ", " ++ show (map (show . i) parents)
          else RevisionInfo {revision = Revision {ident = ident (revision (getRevisionInfo merged)), number = rev'},
                             parentRevisions = map (number . revision . getRevisionInfo) parents,
                             isHead = True}
      -- The new revision number is one greater than the greatest
      -- revision in the head set, or else 1.
      rev' = 1 + foldr max 0 (map (number . revision . getRevisionInfo) (toList s))
      -- unHead :: Revisable a => a -> a
      unHead x = putRevisionInfo ((getRevisionInfo x) {isHead = False}) x
      del rev ix = union (ix @> rev) (ix @< rev) 
      i = ident . revision . getRevisionInfo

-- |Remove all the nodes from all which are (1) in s, (2) not heads,
-- and (3) not common ancestors of heads.  This is a garbage collector
-- for a simple revision control system.  However, you can't use unless
-- you know there are no pending revisions out there waiting to happen.
prune :: forall a. forall b. (Ord a, Data a, Revisable a, Indexable a b) => IxSet a -> IxSet a -> IxSet a
prune s all =
    foldr remove (foldr reParent all reparentPairs) (S.toList victims)
    where
      (reparentPairs, victims) = 
          P.prune s (commonAncestors s (filter (isHead . getRevisionInfo) (toList s)))
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
heads :: (Ord a, Data a, Revisable a, Indexable a b) => IxSet a ->  Heads a
heads s =
    case filter (isHead . getRevisionInfo) (toList s) of
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
combine3 :: (Revisable a, Data a) => (GenericQ (GenericQ Bool)) -> a -> a -> a -> Maybe a
combine3 eq original left right =
    -- Here we make sure the three revision info fields match, because
    -- we are trying to decide whether these revisions can be merged.
    -- It would be nice to modify eq so it considers all RevisionInfos
    -- to be equal, but we don't actually have the RevisionInfo type
    -- here, only a typeclass, so we first need syb-with-class support.
    mergeBy eq original (putRevisionInfo rev left) (putRevisionInfo rev right)
    where rev = getRevisionInfo original

-- Example implementation of the eq argument to combine3.
eqEx :: GenericQ (GenericQ Bool)
eqEx x y =
    (geq `mkQ2` bsEq `extQ2` stringEq) x y
    where
      -- If the specialized eqs don't work, use the generic.  This
      -- will throw an exception if it encounters something with a
      -- NoRep type.
      geq :: (Data a, Data b) => a -> b -> Bool
      geq x y = (toConstr x == toConstr y) && and (gzipWithQ eqEx x y)
      stringEq :: String -> String -> Bool
      stringEq a b = (a == b)
      bsEq :: B.ByteString -> B.ByteString -> Bool
      bsEq a b = (a == b)

combine3traced :: (Revisable a, Data a) => (GenericQ (GenericQ Bool)) -> a -> a -> a -> Maybe a
combine3traced eq original left right =
    mergeByTraced conflict eq original (putRevisionInfo rev left) (putRevisionInfo rev right)
    where rev = getRevisionInfo original
          conflict o l r = trace ("conflict:" ++
                                  "\n original=" ++ show (getRevisionInfo o) ++
                                  "\n left=" ++ show (getRevisionInfo l) ++
                                  "\n right=" ++ show (getRevisionInfo r)) Nothing

-- |Use combine3 to merge as many of the elements in heads as
-- possible, returning the new list.  Consider the mergeable relation
-- on the set of heads.  It is reflexive, symmetric, and transitive,
-- thus an equivalence relation or partition.  We first want to group
-- the nodes into equivalence classes and then perform the mergers on
-- the nodes resulting in a single node per equivalence class.
combine :: (Revisable a, Data a, POSet (IxSet a) a, Indexable a b) =>
           (GenericQ (GenericQ Bool)) -> IxSet a -> IxSet a -> [a] -> (IxSet a, [a])
combine _ all _ [] = (all, [])
combine eq all s heads =
    (all', heads')
    where
      (all', _, heads') = foldr combineClass (all, s, []) (classes eq s heads)
      -- Combine the elements of an equivalence class into a single
      -- new head element.
      combineClass :: (Revisable a, Data a, POSet (IxSet a) a, Indexable a b) =>
                      [a] -> (IxSet a, IxSet a, [a]) -> (IxSet a, IxSet a, [a])
      combineClass parents@(x0 : xs) (all, s, heads) =
          (all', s', merged : heads)
          where (all', s', merged) = merge' all s parents combined'
                -- We removed the revision info so it wouldn't cause
                -- mismatches, now we need to put it back.
                combined' = putRevisionInfo (defaultValue {revision = defaultValue {ident = ident . revision . getRevisionInfo $ x0}}) combined
                -- Combine the class elements into a single element
                combined = foldr (combinePair s) x0 xs
      combineClass [] _ = error "combine: empty equivalence class!"
      -- Combine two elements of the equivalence class into one, ignoring
      -- revision information for now.
      combinePair :: (Revisable a, Data a, POSet (IxSet a) a, Indexable a b) => IxSet a -> a -> a -> a
      combinePair s x y = fromJust (combine3 eq original (unRev x) (unRev y))
          where original = maybe (error message) unRev (commonAncestor s x y)
                message = "no common ancestor: " ++ show (getRevisionInfo x) ++ ", " ++ show (getRevisionInfo y)
      unRev x = putRevisionInfo defaultValue x

combineInfo :: (Revisable a, Data a, POSet (IxSet a) a, Indexable a b) =>
               (GenericQ (GenericQ Bool)) -> IxSet a -> IxSet a -> a -> a -> String
combineInfo eq _all s x y =
    ("Rev 1: " ++ show (getRevisionInfo x)
     ++ ", Rev 2: " ++ show (getRevisionInfo y)
     ++ ", Ancestor: " ++ show (getRevisionInfo a)
     ++ ", Combined: " ++ maybe "None" (show . getRevisionInfo) x')
    where
      x' = combine3 eq a x y
      a = commonAncestor' s x y

classes :: (Revisable a, Indexable a b, Data a, POSet (IxSet a) a) => (GenericQ (GenericQ Bool)) -> IxSet a -> [a] -> [[a]]
classes eq s xs = 
    case xs of
      [] -> []
      (x : xs) -> let (xs', ys) = partition (combines s x) xs in
                  (x : xs') : classes eq s ys
    where
      combines s x y = isJust $ combine3 eq (commonAncestor' s x y) x y

showRev :: RevisionInfo -> String
showRev r = (show . unIdent . ident . revision $ r) ++ "." ++ (show . number . revision $ r) ++ " " ++ show (parentRevisions r)

-- |A version of common ancestor that assumes there is one.
commonAncestor' :: (Revisable a, Data a, POSet (IxSet a) a) => IxSet a -> a -> a -> a
commonAncestor' s x y = maybe (error $ message s x y) id (commonAncestor s x y)

message :: (Revisable a, Data a, POSet (IxSet a) a) => IxSet a -> a -> a -> String
message s x y =
    ("No common ancestor: " ++ showRev rx ++ ", " ++ showRev ry ++
     "\n  parents " ++ showRev rx ++ " -> " ++ intercalate " " (map (showRev . getRevisionInfo) (parents s x))  ++
     "\n  parents " ++ showRev ry ++ " -> " ++ intercalate " " (map (showRev . getRevisionInfo) (parents s y))  ++
     "\n  s = [" ++ intercalate " " (map (showRev . getRevisionInfo) (toList s)) ++ "]" ++
     "\n  commonAncestors -> " ++ intercalate " " (map (showRev . getRevisionInfo) (S.toList (commonAncestors s [x, y]))))
    where rx = getRevisionInfo x
          ry = getRevisionInfo y
