{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses,
             ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, UndecidableInstances #-}
{-# OPTIONS -fno-warn-orphans #-}
-- |These are the pure operations on Store instances, which are
-- typically IxSets of Revisable objects used in a Happstack database.
-- Each element has an Ident, which is stored in a Revision object
-- along with an integer revision number.  There is also bookkeeping
-- storage to record the largest Ident in the store, and a map from
-- Ident to the largest revision number for that Ident.
module Happstack.Data.IxSet.Store
    ( Store(..)
    , Triplet(..)
    , getNextId
    , getNextRev
    -- , getMaxRev
    -- , putMaxRev
    , askHeads
    , askRev
    , askAllRevs
    , askTriplets
    , askAllHeads
    , askAll
    --, reviseElt
    , create
    , reviseAndMerge
    --, replaceElts
    , combineHeads
    , combineHeadsA
    , deleteRev
    , prune
    , setStatus
    , replace
    , replace1
    , replaceA
    , replace1A
    , close
    ) where

import Control.Applicative (Applicative(..))
--import Control.Applicative.Error (Failing(Success, Failure))
import Control.Monad (MonadPlus(..))
import Data.Data (Data)
import Data.Function (on)
import Data.Generics (gshow)
import Data.List (tails, groupBy, sortBy, intercalate)
import qualified Data.Map as M
import Data.Maybe (catMaybes, isJust, isNothing)
import Data.Typeable (Typeable)
import Happstack.Data (deriveSerialize, Default(..), deriveAll)
import Happstack.Data.IxSet (Indexable(..), IxSet(..), (@=), (@+), toList, fromList, delete, insert, null, size)
import Happstack.Data.IxSet.Extra (difference)
import Happstack.Data.IxSet.Merge (threeWayMerge, continue, threeWayMergeA, continueA)
import Happstack.Data.IxSet.POSet (commonAncestor)
import Happstack.Data.IxSet.Revision (Revisable(getRevisionInfo, putRevisionInfo), initialRevision,
                                      RevisionInfo(RevisionInfo, created, revision, parentRevisions),
                                      Revision(ident, number), NodeStatus(Head, NonHead), nodeStatus)
import Happstack.State (EpochMilli, Version)
import Prelude hiding (null)

import Debug.Trace

-- |The Store class, representing a revision controlled collection of elt.
-- FIXME: We probably don't need all this context, particularly Show k, but
-- it means adding Show k to the context of most of the functions below.
class (Ord k, Eq k, Typeable k, Enum k, Default k, Show k, Revisable k elt,
       Indexable elt s, Data elt, Ord elt, Default elt) =>
      Store set k elt s | set -> elt, set -> s where
    getMaxId :: set -> k
    putMaxId :: k -> set -> set
    getMaxRevs :: set -> M.Map k Integer
    putMaxRevs :: M.Map k Integer -> set -> set
    getIxSet :: set -> IxSet elt
    putIxSet :: IxSet elt -> set -> set

getNextId :: (Store set k elt s) => set -> (set, k)
getNextId x = 
    (putMaxId newId x, newId)
    where
      newId = succ oldId
      oldId = getMaxId x

-- |Allocate a new revision number to elt, and update the set to
-- reflect the new maximum revision number.
getNextRev :: (Store set k elt s) => elt -> set -> (set, elt)
getNextRev x set =
    (set', x')
    where
      set' = putMaxRev i n set
      x' = putRevisionInfo (info {revision = r {number = n}}) x
      n = getMaxRev i set + 1
      i = ident r
      r = revision info
      info = getRevisionInfo x

-- |Set the maximum revision number for an Ident.  FIXME - we need a
-- safer way to increase and use the max rev, like getNextId
putMaxRev :: Store set k elt s => k -> Integer -> set -> set
putMaxRev ident rev s = putMaxRevs (M.insert ident rev (getMaxRevs s)) s

-- |Get the maximum revision number in the store for an ident.
getMaxRev :: forall set k elt s. (Store set k elt s, Revisable k elt) => k -> set -> Integer
getMaxRev ident s = 
    maybe getMaxRev' id (M.lookup ident (getMaxRevs s))
    where
      -- If there is no entry for this ident we have to look at all
      -- the revisions in the database for this ident.  This could be
      -- expensive, but it is only needed once, and only for stores
      -- that were created before this max revision code was added.
      getMaxRev' :: Integer
      getMaxRev' = foldr f 0 (toList (getIxSet s @= ident))
      f :: elt -> Integer -> Integer
      f x rev = max rev (number . revision . getRevisionInfo $ x)

-- |The Triplet type represents a possible conflict between two values
-- and their common ancestor (which may be missing if it was already
-- deleted from the database.)
$(deriveAll [''Eq, ''Ord, ''Read, ''Show]
  [d|
      data Triplet a
          = Triplet
            { original :: a
            , left :: a
            , right :: a }
   |])

instance (Ord a, Default a) => Default (Triplet a) where
    defaultValue = Triplet defaultValue defaultValue defaultValue

$(deriveSerialize ''Triplet)
instance Version (Triplet a)

-- |Return a particular revision.
askRev :: (MonadPlus m, Store set k elt s) => (elt -> Maybe elt) -> Revision k -> set -> m elt
askRev scrub rev store =
    case map scrub (toList (getIxSet store @= (trace ("  askRev " ++ show rev) rev))) of
      [] -> fail ("askRev: no such revision: " ++ show rev)
      [Just x] -> return $ trace ("  askRev -> " ++ show (getRevisionInfo x)) x
      [Nothing] -> fail "askRev: permission denied"
      xs -> fail ("askRev: duplicate revisions: " ++ show (map getRevisionInfo (catMaybes xs)))

-- |Return all the revisions for a given ident.
askAllRevs :: (Store set k elt s, Revisable k elt) => (elt -> Maybe elt) -> k -> set -> [Maybe elt]
askAllRevs scrub i store = map scrub (toList ((getIxSet store) @= i))

-- |Return all the head revisions for a given ident.
askHeads :: (Store set k elt s) => (elt -> Maybe elt) -> k -> set -> [Maybe elt]
askHeads scrub i store =
    trace ("  askHeads -> " ++ show (map (fmap getRevisionInfo) xs)) xs
    where
      xs = map scrub (toList xis)
      xis = (getIxSet store) @= Head @= (trace ("  askHeads " ++ show i) i)

-- |Return all the heads for all the idents in the store.
askAllHeads :: (Store set k elt s) => (elt -> Maybe elt) -> set -> [Maybe elt]
askAllHeads scrub = map scrub . toList . (\ s -> s @= Head) . getIxSet

-- |Return all the revisions for all the idents in the store.
askAll :: (Store set k elt s) => (elt -> Maybe elt) -> set -> [Maybe elt]
askAll scrub = map scrub . toList . getIxSet

-- |Return an item's list of (original, left, right) triplets - the
-- list of pairs of head elements, with the common ancestor.
askTriplets :: (Store set k elt s) => (elt -> Maybe elt) -> k -> set -> [Maybe (Triplet elt)]
askTriplets scrub i store =
    {- trace ("  askTriplets " ++ show i ++ " -> " ++ intercalate ", " (map (maybe "Nothing" showTriplet) result)) -} result
    where
      result = triples (commonAncestor xis) heads
      heads = toList (xis @= Head)
      -- This is going to be slow if there are a lot of revisions, but
      -- it is required by the commonAncestor function.  This is why
      -- the revision set needs to be pruned.
      xis = (getIxSet store) @= (trace ("  askTriplets ident=" ++ show i) i)
      -- Build the list of triples.  G is the "nearest common
      -- ancestor" function for this ident.  If there are many
      -- revisions associated with the ident this will be slow.
      triples g xs = concatMap f (tails xs)
          where
            f [] = []
            f (x : xs) =
                -- Note that if the common ancestor is Nothing we need
                -- to do a two way merge.  If the common ancestor is
                -- Nothing because it is scrubbed, we don't want to
                -- try a merge, some other user might be able to
                -- access it and do a three way merge.
                map (\ y -> 
                         case g x y of
                           Just z ->
                               case (scrub x, scrub y, scrub z) of
                                 (Just x', Just y', Just z') -> Just (Triplet {original=z', left=x', right=y'})
                                 _ -> Nothing
                           Nothing ->
                               error "Missing common ancestor"
{-
                               case (scrub x, scrub y) of
                                 (Just x', Just y') -> Just (Triplet {original=Nothing, left=x', right=y'})
                                 _ -> Nothing
-}
                    ) xs

-- |Create a new revision of an existing element, and then try to
-- merge all the heads.
reviseAndMerge :: (MonadPlus m, Store set k elt s) =>
                  (elt -> Maybe elt) -> (elt -> elt) -> EpochMilli -> [Revision k] -> elt -> set -> m (Maybe set, elt, [elt])
reviseAndMerge scrub prep creationTime revs x store =
    if all isJust xs
    then replace1 scrub creationTime revs x store >>=
             \ (store', x') ->
               let i = trace ("  reviseAndMerge " ++ show revs) (ident (revision (getRevisionInfo x'))) in
               combineHeads scrub prep i creationTime store' >>= 
                            \ (store'', heads) ->
                                return (maybe (Just store') Just store'', x', heads)
    else fail "reviseAndMerge: permission denied"
    where
      xs = map scrub (toList (set @+ revs))
      set = getIxSet store

create :: (MonadPlus m, Store set k elt s) => (elt ->Maybe elt) -> EpochMilli -> elt -> set -> m (set, elt)
create scrub creationTime x store =
    let (store', i) = getNextId store in
    let x' = initialRevision i creationTime x in
    replace scrub creationTime [] [x'] store' >>=
            \ (store'', xs) -> case xs of
                                 [x''] -> return (store'', x'')
                                 _ -> fail "replace failed"

-- |Examine the set of head revisions and attempt to merge as many as
-- possible using the automatic threeWayMerge function.  Returns the
-- new list of heads.  The modified store is returned only if changes
-- were made.
combineHeads :: forall m set k elt s. (MonadPlus m, Store set k elt s) =>
                (elt -> Maybe elt) -> (elt -> elt) -> k -> EpochMilli -> set -> m (Maybe set, [elt])
combineHeads scrub prep i creationTime set =
    merge False set (askTriplets scrub i set)
    where
      -- No triplets left to merge, return the finalized list of heads
      merge :: Bool -> set -> [Maybe (Triplet elt)] -> m (Maybe set, [elt])
      merge merged set [] =
          return (if merged then Just set else Nothing, heads)
          where heads = toList ((getIxSet set @= i) @= Head)
      -- Try to merge each of the triplets in turn
      merge merged set (Just (Triplet o l r) : more) =
          threeWayMerge continue (prep' o) (prep' l) (prep' r) >>=
            \ m -> replace1 scrub creationTime [lrev, rrev] m set >>=
            \ (set', _) -> merge True set' (askTriplets scrub i set')
          where
            orev = revision (getRevisionInfo o)
            lrev = revision (getRevisionInfo l)
            rrev = revision (getRevisionInfo r)
      -- Permission failure
{-    merge _ _ (Just (Triplet _ l r) : _) =
          fail ("combineHeads: missing ancestor of " ++ show [getRevisionInfo l, getRevisionInfo r]) -}
      merge merged set (Nothing : more) = merge merged set more
      prep' = clearRev . prep

combineHeadsA :: forall f set k elt s. (Applicative f, Store set k elt s) =>
                (elt -> Maybe elt) -> (elt -> elt) -> k -> EpochMilli -> set -> f (Maybe set, [elt])
combineHeadsA scrub prep i creationTime set =
    merge False set (askTriplets scrub i set)
    where
      -- No triplets left to merge, return the finalized list of heads
      merge :: Bool -> set -> [Maybe (Triplet elt)] -> f (Maybe set, [elt])
      merge merged set [] =
          pure (if merged then Just set else Nothing, heads)
          where heads = toList ((getIxSet set @= i) @= Head)
      -- Try to merge each of the triplets in turn
      merge merged set (Just (Triplet o l r) : more) =
          undefined
{-
          twoOrThreeWayMerge continue (fmap prep' o) (prep' l) (prep' r) >>=
            \ m -> replace1 scrub creationTime [lrev, rrev] m set >>=
            \ (set', _) -> merge True set' (askTriplets scrub i set')
-}
          where
            orev = revision (getRevisionInfo o)
            lrev = revision (getRevisionInfo l)
            rrev = revision (getRevisionInfo r)
      -- Permission failure
{-    merge _ _ (Just (Triplet _ l r) : _) =
          error ("combineHeads: missing ancestor of " ++ show [getRevisionInfo l, getRevisionInfo r]) -}
      merge merged set (Nothing : more) = merge merged set more
      prep' = clearRev . prep

conflict = undefined

clearRev :: forall k a. (Revisable k a, Default k) => a -> a
clearRev x =
    putRevisionInfo ((defaultValue :: RevisionInfo k) {revision = (defaultValue :: Revision k) {ident = ident (revision (getRevisionInfo x))}}) x

_copyRev :: forall k a. (Revisable k a, Default k) => a -> a -> a
_copyRev src dst = putRevisionInfo (getRevisionInfo src) dst

-- |Change the node status of a revision to Head or NonHead.
setStatus :: forall m set k elt s. (MonadPlus m, Store set k elt s) =>
             (elt -> Maybe elt) -> NodeStatus -> Revision k -> set -> m (set, elt)
setStatus scrub status rev store =
    let xs = getIxSet store :: IxSet elt
        xis = xs @= ident rev :: IxSet elt
        xos = (toList $ xis @= rev) :: [elt] in
    case map scrub xos of
      [Just xo] -> 
          let xs' = delete xo xs in
          let xo' = putRevisionInfo ((getRevisionInfo xo) {nodeStatus = status}) xo in
          return (putIxSet (insert xo' xs') store, xo')
      [Nothing] -> fail "Permission denied"
      [] -> fail ("Not found: " ++ show rev)
      xs -> fail ("Duplicate revisions: " ++ show (map getRevisionInfo (catMaybes xs)))

-- |Delete a revision from the store, and remove its revision number
-- from all parent lists.  Return the new head, if there still is one.
-- Note the distinction between this and closing a revision, which
-- leaves it in the store but sets its status to NonHead without
-- creating any children.
deleteRev :: forall m set k elt s. (MonadPlus m, Store set k elt s) =>
             (elt -> Maybe elt) -> Revision k -> set -> m set
deleteRev scrub rev store =
    case map scrub xos of
      [] -> fail "Not found"
      [Nothing] -> fail "Permission denied"
      [Just xo] ->
          let number' = number . revision . getRevisionInfo
              parentRevisions' = parentRevisions . getRevisionInfo
              setParentRevisions revs x = putRevisionInfo ((getRevisionInfo x) {parentRevisions = revs}) x
              isHead' = (== Head) . nodeStatus . getRevisionInfo
              setHead flag x = putRevisionInfo ((getRevisionInfo x) {nodeStatus = flag}) x
              replace old new set = insert new (delete old set)
              -- In the parentRevisions list is the node or nodes
              -- which were revised to create this node.  Therefore,
              -- when a head node is deleted, the nodes in its parent
              -- list will become heads.  If the victim node appears
              -- in any parent lists, it is replaced by the nodes in
              -- its parent list.
              xs' = foldr f (delete xo xs) (toList xis)
                       where f x xs = 
                                 if elem (number' x) (parentRevisions' xo) && isHead' xo && not (isHead' x)
                                 -- If the victim node was a head, its parents will now be heads
                                 then replace x (setHead Head x) xs
                                 else if elem (number' xo) (parentRevisions' x)
                                      -- Remove the victim node from the parent list and add the victim's parent list
                                      then replace x (setParentRevisions
                                                      (filter (/= (number' xo)) (parentRevisions' x) ++
                                                       parentRevisions' xo) x) xs
                                      else xs in
          return (putIxSet xs' store)
      _ -> fail "Conflict"
    where
      xs = getIxSet store :: IxSet elt
      xis = xs @= ident rev :: IxSet elt
      xos = (toList $ xis @= rev) :: [elt]

-- |Prune (delete) any elements that aren't heads or a closest
-- common ancestor of some pair of heads.  Note that some revision
-- that is not a head may be sitting in a form in somebody's browser,
-- and when they edit and submit that form they could create a new
-- head which has ancestors this function would have deleted.  There
-- are several potential solutions to this problem, the simplest is to
-- implement two way merging.
prune :: forall m set k elt s. (MonadPlus m, Store set k elt s) =>
         (elt -> Maybe elt) -> k -> set -> m (Maybe set)
prune scrub i store =
    if any isNothing triplets
    then fail "Permission denied"
    else if any isNothing keep
         then fail "Permission denied"
         else return (if null discard
                      then Nothing
                      else (Just $ putIxSet (difference set discard) store))
    where
      discard :: IxSet elt
      discard = difference all (fromList (catMaybes keep))
      keep :: [Maybe elt]
      keep = askHeads Just i store ++ fmap scrub ancestors
      ancestors = map original (catMaybes triplets)
      all :: IxSet elt
      all = set @= i
      triplets :: [Maybe (Triplet elt)]
      triplets = askTriplets scrub i store
      set = getIxSet store

-- |Declare MERGED to be the child of PARENTS - allocate a new
-- revision, put it into MERGED's revision field with the list of
-- parents, set its nodeStatus to Head, and set the nodeStatus of all
-- the parents to NonHead.  Note that this can be used to create a
-- revision (by passing an empty parent list), revise a single item,
-- or merge several items.  It can also be used to create a branch 
-- by revising an element that already has children.
replace1 :: forall m set k elt s. (MonadPlus m, Store set k elt s, Indexable elt s) =>
            (elt -> Maybe elt) -> EpochMilli -> [Revision k] -> elt -> set -> m (set, elt)
replace1 scrub creationTime parentRevs merged store =
    replace scrub creationTime parentRevs [merged] store >>=
            \ (store', merged') -> 
                case merged' of
                  [merged''] -> return (store', merged'')
                  _ -> fail "Unexpected result from replace"

replace1A :: forall f set k elt s. (Applicative f, Store set k elt s, Indexable elt s) =>
            (elt -> Maybe elt) -> EpochMilli -> [Revision k] -> set -> elt -> f (set, elt)
replace1A scrub creationTime parentRevs store merged =
    fmap finish (replaceA scrub creationTime parentRevs [merged] store)
    where
      finish (store', [merged']) = (store', merged')
      finish _ = error "Unexpected result from replaceA"

-- |Replace zero or more parents with zero or more children.
replace :: forall m set k elt s. (MonadPlus m, Store set k elt s, Indexable elt s) =>
           (elt -> Maybe elt) -> EpochMilli -> [Revision k] -> [elt] -> set -> m (set, [elt])
replace scrub creationTime parentRevs children store =
    case parentIds ++ childIds of
      [] -> fail "replace: No parents and no children"
      ids@(i : _) | allEqual ids -> replace' scrub i creationTime parentRevs children store
      _ids -> fail ("replace: id mismatch: parentIds=" ++ show parentIds ++ ", childIds=" ++ show childIds)
    where
      childIds = map (ident . revision . getRevisionInfo) children
      parentIds = map ident parentRevs

-- |Replace zero or more parents with zero or more children.  Should be pure.
replaceA :: forall f set k elt s. (Applicative f, Store set k elt s, Indexable elt s) =>
           (elt -> Maybe elt) -> EpochMilli -> [Revision k] -> [elt] -> set -> f (set, [elt])
replaceA scrub creationTime parentRevs children store =
    case parentIds ++ childIds of
      [] -> error "replace: No parents and no children"
      ids@(i : _) | allEqual ids -> replaceA' scrub i creationTime parentRevs children store
      _ids -> error ("replace: id mismatch: parentIds=" ++ show parentIds ++ ", childIds=" ++ show childIds)
    where
      childIds = map (ident . revision . getRevisionInfo) children
      parentIds = map ident parentRevs

-- |This is the internal function that does the work for replace,
-- replace1, and close.  This fails if we can't access any of the
-- parents.
replace' :: forall m set k elt s. (MonadPlus m, Store set k elt s, Indexable elt s) =>
            (elt -> Maybe elt) -> k -> EpochMilli -> [Revision k] -> [elt] -> set -> m (set, [elt])
replace' scrub i creationTime parentRevs children store =
    case any isNothing parents of
      True -> fail "replace: Permission denied"
      False -> if size set'' == size set' + length children'
               then return (store'', children')
               else fail ("Failed to insert " ++ show childRevs' ++ " into " ++ gshowSet set' ++ ": result was " ++ gshowSet set'')
    where
      store'' :: set
      store'' = putMaxRev i (getMaxRev i store' + toInteger (length children)) store'
      store' :: set
      store' = putIxSet set'' store
      -- Insert the new children
      set'' :: IxSet elt
      set'' = foldr insert set' children'
      -- Change the status of all the parents to NonHead
      set' :: IxSet elt
      set' = foldr unHead set (catMaybes parents)
      parents :: [Maybe elt]
      parents = map scrub (toList (set @+ parentRevs))
      children' :: [elt]
      children' = map (uncurry putRevisionInfo) (zip childInfo children)
      childInfo :: [RevisionInfo k]
      childInfo = map (\ rev -> RevisionInfo {revision = rev,
                                              created = creationTime,
                                              parentRevisions = map number parentRevs,
                                              nodeStatus = Head}) childRevs'
      childRevs' :: [Revision k]
      childRevs' = map (\ (rev, n) -> rev {number = n + getMaxRev i store}) (zip childRevs [1..])
      childRevs :: [Revision k]
      childRevs = map (revision . getRevisionInfo) children
      set :: IxSet elt
      set = getIxSet store
      unHead x xs = insert x' (delete x xs)
          where x' = putRevisionInfo (f (getRevisionInfo x)) x
                f :: RevisionInfo k -> RevisionInfo k
                f x = x {nodeStatus = NonHead}

-- |This is the internal function that does the work for replace,
-- replace1, and close.  This fails if we can't access any of the
-- parents.  (This should be pure.)
replaceA' :: forall f set k elt s. (Applicative f, Store set k elt s, Indexable elt s) =>
            (elt -> Maybe elt) -> k -> EpochMilli -> [Revision k] -> [elt] -> set -> f (set, [elt])
replaceA' scrub i creationTime parentRevs children store =
    case any isNothing parents of
      True -> error "replace: Permission denied"
      False -> if size set'' == size set' + length children'
               then pure (store'', children')
               else error ("Failed to insert " ++ show childRevs' ++ " into " ++ gshowSet set' ++ ": result was " ++ gshowSet set'')
    where
      store'' :: set
      store'' = putMaxRev i (getMaxRev i store' + toInteger (length children)) store'
      store' :: set
      store' = putIxSet set'' store
      -- Insert the new children
      set'' :: IxSet elt
      set'' = foldr insert set' children'
      -- Change the status of all the parents to NonHead
      set' :: IxSet elt
      set' = foldr unHead set (catMaybes parents)
      parents :: [Maybe elt]
      parents = map scrub (toList (set @+ parentRevs))
      children' :: [elt]
      children' = map (uncurry putRevisionInfo) (zip childInfo children)
      childInfo :: [RevisionInfo k]
      childInfo = map (\ rev -> RevisionInfo {revision = rev,
                                              created = creationTime,
                                              parentRevisions = map number parentRevs,
                                              nodeStatus = Head}) childRevs'
      childRevs' :: [Revision k]
      childRevs' = map (\ (rev, n) -> rev {number = n + getMaxRev i store}) (zip childRevs [1..])
      childRevs :: [Revision k]
      childRevs = map (revision . getRevisionInfo) children
      set :: IxSet elt
      set = getIxSet store
      unHead x xs = insert x' (delete x xs)
          where x' = putRevisionInfo (f (getRevisionInfo x)) x
                f :: RevisionInfo k -> RevisionInfo k
                f x = x {nodeStatus = NonHead}

-- |Close some revisions without creating any children.
close :: forall m set k elt s. (MonadPlus m, Store set k elt s, Indexable elt s) =>
         (elt -> Maybe elt) -> [Revision k] -> set -> m (set)
close scrub revs store = replace scrub 0 revs [] store >>= return . fst

-- Utility functions.

-- |Look for cases where the same revision exists in the database
-- twice, once with status Head and once with status NonHead.  This
-- should never happen, but it did once due to a bug, and this was
-- used to repair the database.
_fixBadRevs :: forall set k elt s. (Store set k elt s, Revisable k elt) => k -> set -> set
_fixBadRevs i store =
    foldr repair store (concat bad)
    where 
      repair x store =
          store''
          where
            store'' = putIxSet ix' store'
            ix' = insert x' (delete x ix)
            ix = getIxSet store'
            x' = putRevisionInfo (trace ("  Changing revision from " ++ show info ++ " to " ++ show info') info') x
            info' = info {revision = (revision info) {number = rev}, nodeStatus = Head}
            info = getRevisionInfo x
            store' = putMaxRev i rev store
            rev = 1 + getMaxRev i store
      bad = filter (\ g -> length g > 1) 
                (groupBy ((==) `on` (number . revision . getRevisionInfo))
                 (sortBy (compare `on` (number . revision . getRevisionInfo)) revs))
      revs = toList (set @= i)
      set = getIxSet store

_showTriplet :: (Revisable k a, Show k) => Triplet a -> String
_showTriplet (Triplet o l r) = "Triplet {o=" ++ (show . revision . getRevisionInfo $ o) ++
                               ", l=" ++ (show . revision . getRevisionInfo $ l) ++
                               ", r=" ++ (show . revision . getRevisionInfo $ r) ++ "}"

gshowSet :: (Ord a, Data a) => IxSet a -> String
gshowSet s = gshowList (toList s)
gshowList :: (Data a) => [a] -> [Char]
gshowList l = "[" ++ intercalate ", " (map gshow l) ++ "]"

traceThis :: (a -> String) -> a -> a
traceThis f x = trace (f x) x

allEqual :: Eq a => [a] -> Bool
allEqual (x : more) = all (\ y -> x == y) more
allEqual [] = True

_traceRev :: (Revisable k a, Show k) => String -> a -> a
_traceRev prefix x = trace (prefix ++ show (getRevisionInfo x)) x
_traceRevs :: (Revisable k a, Show k) => String -> [a] -> [a]
_traceRevs prefix xs = trace (prefix ++ show (map getRevisionInfo xs)) xs
