{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances,
             MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, TemplateHaskell,
             UndecidableInstances #-}
{-# OPTIONS -fno-warn-orphans -Wwarn #-}
module Happstack.Data.IxSet.Revision
    ( Ident(..)
    , Revision(..)
    , RevisionInfo(..)
    , RevisionInfo001(..)
    , Revisable(..)
    , NodeStatus(..)
    , copyRev
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
    , combine3M
    , conflicts
    , eqEx
    ) where

--import Control.Applicative.Error (Failing(..))
import qualified Data.ByteString.Char8 as B
import Data.Generics
import Data.List (tails, partition, intercalate)
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as S
import Happstack.Data (Default(..), deriveNewData, deriveSerialize, Migrate(..))
import Happstack.Data.IxSet
--import Text.Formlets (xml, check)
--import Text.Formlets.Generics.Instances ()
--import Text.Formlets.Generics.Markup.Types (Markup(..))
import Happstack.Data.IxSet.POSet
import qualified Happstack.Data.IxSet.POSet as P
import Happstack.Data.IxSet.Triplets (mergeBy, mergeByM, mkQ2, extQ2, gzipBut3)
import Happstack.State (Version(..), Proxy(..), Mode(..), extension, proxy)

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
    deriving (Eq, Ord, Read, Data, Typeable)

-- | The information associated with a revision to record its status.
data RevisionInfo
    = RevisionInfo {revision :: Revision, parentRevisions :: [Integer], nodeStatus :: NodeStatus}
    deriving (Eq, Ord, Read, Data, Typeable)

instance Show RevisionInfo where
    show r = show (revision r) ++
             if nodeStatus r == Head then " (Head)" else " (NonHead)"

instance Show Revision where
    show r = "Rev. " ++ show (unIdent (ident r)) ++ "." ++ show (number r)

data NodeStatus = Head | NonHead deriving (Eq, Ord, Read, Show, Data, Typeable)

-- |Class of values that have a revision info.
class Revisable a where
    getRevisionInfo :: a -> RevisionInfo
    putRevisionInfo :: RevisionInfo -> a -> a

copyRev :: (Revisable a, Revisable b) => a -> b -> b
copyRev s d = putRevisionInfo (getRevisionInfo s) d

changeRevisionInfo :: Revisable a => (RevisionInfo -> RevisionInfo) -> a -> a
changeRevisionInfo f x = putRevisionInfo (f (getRevisionInfo x)) x

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
                                   nodeStatus = Head}) x

{-
merge :: (Ord a, Data a, Revisable a, Indexable a b) => IxSet a -> Ident -> [a] -> a -> (IxSet a, a)
merge all i parents merged =
    if any ((/= Head) . nodeStatus . getRevisionInfo) parents
    then error "Attempt to merge non-head revision"
    merge' all i parents merged
-}

-- |Revise is a special case of merge, where we replace a single
-- element (rather than several) with one new element.
revise :: (Revisable a, Ord a, Data a, Indexable a b) => IxSet a -> a -> a -> (IxSet a, a)
revise all parent revised = merge all [parent] revised

-- |Insert a revision into the index and designate it the merger of a
-- list of existing revisions.  The status of the new revision will be
-- set to Head, the status of the parents set to NonHead.  Note that
-- this can be used to create a revision (by passing an empty parent
-- list), revise a single item, or merge several items.
merge :: forall a. forall b. (Ord a, Data a, Revisable a, Indexable a b) =>
          IxSet a -> [a] -> a -> (IxSet a, a)
merge all parents merged =
    if any (/= ident rev) (map ident parentRevs)
    then error $ "merge: ident mismatch: merged=" ++ show (ident rev) ++ ", parents=" ++ show parentRevs
    else (all'', trace ("merge: merged'=" ++ show (getRevisionInfo merged')) merged')
    where
      -- Add the 
      all'' = insert merged' all'
      all' = foldr unHead all parents
      unHead :: a -> IxSet a -> IxSet a
      unHead x xs = insert x' (delete x xs)
          where x' :: a
                x' = changeRevisionInfo f x
                f :: RevisionInfo -> RevisionInfo
                f x = x {nodeStatus = NonHead}
      -- Put the new revision info into the merged element
      merged' = putRevisionInfo info' merged
      -- Create the new revision info
      info' = RevisionInfo {revision = rev', parentRevisions = map number parentRevs, nodeStatus = Head}
      -- The new revision number is one greater than the greatest
      -- revision in the head set, or else 1.  FIXME: We are assuming
      -- that there is no non-head revision greater than the maximum
      -- head revision.  We actually need to store the maxRevision with
      -- the report store, as we do maxIdent.
      rev' = rev {number = 1 + foldr max 0 (map (number . revision . getRevisionInfo) (toList heads))}
          where heads = all @= ident rev @= Head
      parentRevs = map (revision . getRevisionInfo) parents
      rev = revision . getRevisionInfo $ merged

-- |Remove all the nodes from all which are (1) in s, (2) not heads,
-- and (3) not common ancestors of heads.  This is a garbage collector
-- for a simple revision control system.  However, you can't use unless
-- you know there are no pending revisions out there waiting to happen.
prune :: forall a. forall b. (Ord a, Data a, Revisable a, Indexable a b) => IxSet a -> IxSet a -> IxSet a
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
heads :: (Ord a, Data a, Revisable a, Indexable a b) => IxSet a ->  Heads a
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
combine3 :: forall a. (Revisable a) => (a -> a -> a -> Maybe a) -> (a -> a -> Bool) -> a -> a -> a -> Maybe a
combine3 conflict eq original left right =
    mergeBy conflict eq original (putRevisionInfo rev left) (putRevisionInfo rev right)
    where rev = getRevisionInfo original

type GB = GenericQ (GenericQ (GenericQ Bool))

conflicts :: forall a. (Revisable a, Data a) =>
             GB -> (forall x. Data x => x -> x -> x -> Maybe x) -> (forall x. Data x => x -> x -> Bool) -> a -> a -> a -> Maybe a
conflicts q conflict eq original left right =
    gzipBut3 merge q original left right
    where
      merge :: forall x. Data x => x -> x -> x -> Maybe x
      merge o l r = 
          if eq o l
          then Just r
          else if eq o r
               then Just l
               else if eq l r
                    then Just l
                    else conflict o l r

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

combine3M :: forall m a. (Revisable a, Monad m) => (a -> a -> a -> m (Maybe a)) -> (a -> a -> Bool) -> a -> a -> a -> m (Maybe a)
combine3M conflict eq original left right =
    mergeByM conflict eq original (putRevisionInfo rev left) (putRevisionInfo rev right)
    where rev = getRevisionInfo original

-- |Use combine3 to merge as many of the elements in heads as
-- possible, returning the new list.  Consider the mergeable relation
-- on the set of heads.  It is reflexive, symmetric, and transitive,
-- thus an equivalence relation or partition.  We first want to group
-- the nodes into equivalence classes and then perform the mergers on
-- the nodes resulting in a single node per equivalence class.
combine :: forall a b. (Revisable a, Data a, POSet (IxSet a) a, Indexable a b) =>
           (GenericQ (GenericQ Bool)) -> IxSet a -> [a] -> (IxSet a, [a])
combine _ all [] = (all, [])
combine eq set heads@(head : _) =
    if all (== i) idents
    then (set', heads')
    else error $ "combine: ident mismatch: " ++ show idents
    where
      (set', heads') = foldr combineClass (set, []) (classes eq (set @= i) heads)
      -- Combine the elements of an equivalence class into a single
      -- new head element.
      combineClass :: (Revisable a, Data a, POSet (IxSet a) a, Indexable a b) =>
                      [a] -> (IxSet a, [a]) -> (IxSet a, [a])
      combineClass parents@(x0 : xs) (set, heads) =
          (set', merged : heads)
          where (set', merged) = merge set parents combined'
                -- We removed the revision info so it wouldn't cause
                -- mismatches, now we need to put it back.
                combined' = putRevisionInfo (defaultValue {revision = defaultValue {ident = ident . revision . getRevisionInfo $ x0}}) combined
                -- Combine the class elements into a single element
                combined = foldr (combinePair s) x0 xs
                s = set @= i
      combineClass [] _ = error "combine: empty equivalence class!"
      -- Combine two elements of the equivalence class into one, ignoring
      -- revision information for now.
      combinePair :: (Revisable a, Data a, POSet (IxSet a) a, Indexable a b) => IxSet a -> a -> a -> a
      combinePair s x y = fromJust (combine3 (\ _ _ _ -> Nothing) eq original (unRev x) (unRev y))
          where original = maybe (error message) unRev (commonAncestor s x y)
                message = "no common ancestor: " ++ show (getRevisionInfo x) ++ ", " ++ show (getRevisionInfo y)
      unRev x = putRevisionInfo defaultValue x
      idents = map (ident . revision . getRevisionInfo) heads
      i = ident . revision . getRevisionInfo $ head

combineInfo :: (Revisable a, Data a, POSet (IxSet a) a, Indexable a b) =>
               (GenericQ (GenericQ Bool)) -> IxSet a -> IxSet a -> a -> a -> String
combineInfo eq _all s x y =
    ("Rev 1: " ++ show (getRevisionInfo x)
     ++ ", Rev 2: " ++ show (getRevisionInfo y)
     ++ ", Ancestor: " ++ show (getRevisionInfo a)
     ++ ", Combined: " ++ maybe "None" (show . getRevisionInfo) x')
    where
      x' = combine3 (\ _ _ _ -> Nothing) eq a x y
      a = commonAncestor' s x y

classes :: (Revisable a, Indexable a b, Data a, POSet (IxSet a) a) => (GenericQ (GenericQ Bool)) -> IxSet a -> [a] -> [[a]]
classes eq s xs = 
    case xs of
      [] -> []
      (x : xs) -> let (xs', ys) = partition (combines s x) xs in
                  (x : xs') : classes eq s ys
    where
      combines s x y = isJust $ combine3 (\ _ _ _ -> Nothing) eq (commonAncestor' s x y) x y

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

$(deriveNewData [''Ident])
$(deriveNewData [''Revision])
$(deriveNewData [''NodeStatus])
$(deriveNewData [''RevisionInfo])

---------------
-- MIGRATION --
---------------

-- |Obsolete version of the RevisionInfo structure.  The isHead field
-- was changed to nodeStatus :: NodeStatus.
data RevisionInfo001
    = RevisionInfo001 {revision001 :: Revision, parentRevisions001 :: [Integer], isHead001 :: Bool}
    deriving (Eq, Ord, Read, Show, Data, Typeable)

instance Migrate RevisionInfo001 RevisionInfo where
    migrate r =
        RevisionInfo { revision = revision001 r
                     , parentRevisions = parentRevisions001 r
                     , nodeStatus = if isHead001 r then Head else NonHead }

instance Version RevisionInfo001
$(deriveSerialize ''RevisionInfo001)
instance Version RevisionInfo where mode = extension 1 (proxy undefined :: Proxy RevisionInfo001) :: Mode RevisionInfo
$(deriveSerialize ''RevisionInfo)
instance Version Revision
$(deriveSerialize ''Revision)
instance Version NodeStatus
$(deriveSerialize ''NodeStatus)
instance Version Ident
$(deriveSerialize ''Ident)
