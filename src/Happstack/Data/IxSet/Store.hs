{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell, UndecidableInstances #-}
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
    --, reviseElt
    , reviseAndMerge
    --, replaceElts
    , combineHeads
    , deleteRev
    , prune
    , setStatus
    , replace
    , replace1
    , close
    ) where

import Data.Data (Data)
import Data.Function (on)
import Data.List (tails, groupBy, sortBy, intercalate)
import qualified Data.Map as M
import Data.Maybe (catMaybes, isJust, isNothing)
import Happstack.Data (deriveSerialize, Default(..), deriveAll)
import Happstack.Data.IxSet (Indexable(..), IxSet(..), (@=), (@+), toList, delete, insert)
import Happstack.Data.IxSet.Merge (twoOrThreeWayMerge)
import Happstack.Data.IxSet.POSet (commonAncestor)
import Happstack.Data.IxSet.Revision (Revisable(getRevisionInfo, putRevisionInfo),
                                      RevisionInfo(RevisionInfo, revision, parentRevisions),
                                      Revision(ident, number), Ident(Ident), NodeStatus(Head, NonHead), nodeStatus)
import Happstack.State (Version)

import Debug.Trace

-- |The Store class, representing a revision controlled collection of elt.
class (Revisable elt, Indexable elt s, Data elt, Ord elt) => Store set elt s | set -> elt, set -> s where
    getMaxId :: set -> Ident
    putMaxId :: Ident -> set -> set
    getMaxRevs :: set -> M.Map Ident Integer
    putMaxRevs :: M.Map Ident Integer -> set -> set
    getIxSet :: set -> IxSet elt
    putIxSet :: IxSet elt -> set -> set

getNextId :: (Store set elt s) => set -> (set, Ident)
getNextId x = 
    (putMaxId newId x, newId)
    where
      newId = Ident (oldId + 1)
      (Ident oldId) = getMaxId x

-- |Allocate a new revision number to elt, and update the set to
-- reflect the new maximum revision number.
getNextRev :: (Store set elt s) => elt -> set -> (set, elt)
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
putMaxRev :: Store set elt s => Ident -> Integer -> set -> set
putMaxRev ident rev s = putMaxRevs (M.insert ident rev (getMaxRevs s)) s

-- |Get the maximum revision number in the store for an ident.
getMaxRev :: forall set elt s. (Store set elt s, Revisable elt) => Ident -> set -> Integer
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
            { original :: Maybe a
            , left :: a
            , right :: a }
   |])

instance (Ord a, Default a) => Default (Triplet a) where
    defaultValue = Triplet defaultValue defaultValue defaultValue

$(deriveSerialize ''Triplet)
instance Version (Triplet a)

-- |Return a particular revision.
askRev :: (Store set elt s) => (elt -> Maybe elt) -> Revision -> set -> Maybe elt
askRev scrub rev store =
    case map scrub (toList (getIxSet store @= (trace ("  askRev " ++ show rev) rev))) of
      [] -> trace "askRev -> Nothing" Nothing
      [Just x] -> Just (trace ("  askRev -> " ++ show (getRevisionInfo x)) x)
      [Nothing] -> error "askRev: permission denied"
      xs -> error ("askRev: duplicate revisions: " ++ show (map getRevisionInfo (catMaybes xs)))

-- |Return all the revisions for a given ident.
askAllRevs :: (Store set elt s, Revisable elt) => (elt -> Maybe elt) -> Ident -> set -> [Maybe elt]
askAllRevs scrub i store =
    let xis = (getIxSet store) @= i in
    case map scrub (toList xis) of
      [] -> error $ "askAllRevs - no head: " ++ show (map getRevisionInfo (toList xis))
      xs -> trace ("  askAllRevs -> " ++ show (map (fmap getRevisionInfo) xs)) xs

-- |Return all the head revisions for a given ident.
askHeads :: (Store set elt s) => (elt -> Maybe elt) -> Ident -> set -> [Maybe elt]
askHeads scrub i store =
    let xis = (getIxSet store) @= Head @= (trace ("  askHeads " ++ show i) i) in
    case map scrub (toList xis) of
      [] -> error $ "askHeads - no head: " ++ show (map getRevisionInfo (toList xis))
      xs -> trace ("  askHeads -> " ++ show (map (fmap getRevisionInfo) xs)) xs

-- |Return all the heads for all the idents in the store.
askAllHeads :: (Store set elt s) => (elt -> Maybe elt) -> set -> [Maybe elt]
askAllHeads scrub = map scrub . heads . getIxSet

-- |Return an item's list of (original, left, right) triplets - the
-- list of pairs of head elements, with the common ancestor.
askTriplets :: (Store set elt s) => (elt -> Maybe elt) -> Ident -> set -> [Maybe (Triplet elt)]
askTriplets scrub i store =
    trace ("  askTriplets " ++ show i ++ " -> " ++ intercalate ", " (map (maybe "Nothing" showTriplet) result)) result
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
                                 (Just x', Just y', Just z') -> Just (Triplet {original=Just z', left=x', right=y'})
                                 _ -> Nothing
                           Nothing ->
                               case (scrub x, scrub y) of
                                 (Just x', Just y') -> Just (Triplet {original=Nothing, left=x', right=y'})
                                 _ -> Nothing) xs


-- |Create a new revision of an existing element, and then try to
-- merge all the heads.
reviseAndMerge :: (Store set elt s) => (elt -> Maybe elt) -> [Revision] -> elt -> set -> (Maybe set, elt, Maybe [elt])
reviseAndMerge scrub revs x store =
    if all isJust xs
    then let (store', x') = replace1 scrub revs x store
             i = trace ("  reviseAndMerge " ++ show revs) (ident (revision (getRevisionInfo x'))) in
         case combineHeads scrub i store' of
           Left heads -> (Just store', x', Just heads)
           Right (store'', heads) -> (Just store'', x', Just heads)
    else error "reviseAndMerge: permission denied"
    where
      xs = map scrub (toList (set @+ revs))
      set = getIxSet store

-- |Examine the set of head revisions and attempt to merge as many as
-- possible using the automatic threeWayMerge function.
combineHeads :: forall set elt s. (Store set elt s) =>
                (elt -> Maybe elt) -> Ident -> set -> Either [elt] (set, [elt])
combineHeads scrub i set =
    result
    where
      result = merge False set triplets
      triplets = (askTriplets scrub i set)
      -- No triplets left to merge, return the finalized list of heads
      merge :: Bool -> set -> [Maybe (Triplet elt)] -> Either [elt] (set, [elt])
      merge merged set [] =
          let heads = toList ((getIxSet set @= i) @= Head) in
          if merged then Right (set, heads) else Left heads
      -- Try to merge each of the triplets in turn
      merge merged set (Just (Triplet o@(Just _) l r) : more) =
          let copyRev s d = putRevisionInfo (getRevisionInfo s) d
              o' = fmap (copyRev l) o
              r' = copyRev l r
              orev = fmap (revision . getRevisionInfo) o
              lrev = revision (getRevisionInfo l)
              rrev = revision (getRevisionInfo r) in
          case traceThis (\ m -> "  combineHeads threeWayMerge " ++
                                 "o=" ++ show orev ++
                                 ", l=" ++ show lrev ++
                                 ", r=" ++ show rrev ++
                                 " -> " ++ show (fmap (revision . getRevisionInfo) m))
                   (twoOrThreeWayMerge o' l r') of
            -- We merged a triplet, set the merged flag and re-start the combine process
            Just m -> let (set', _) = replace1 scrub [lrev, rrev] m set in
                      merge True set' (askTriplets scrub i set')
            -- We couldn't merge a triplet, try the next
            Nothing -> merge merged set more
      -- Permission failure
      merge _ _ (Just (Triplet _ l r) : _) =
          error ("combineHeads: missing ancestor of " ++ show [getRevisionInfo l, getRevisionInfo r])
      merge merged set (Nothing : more) = merge merged set more

-- |Change the node status of a revision to Head or NonHead.
setStatus :: forall set elt s. (Store set elt s) => (elt -> Maybe elt) -> NodeStatus -> Revision -> set -> (set, elt)
setStatus scrub status rev store =
    let xs = getIxSet store :: IxSet elt
        xis = xs @= ident rev :: IxSet elt
        xos = (toList $ xis @= rev) :: [elt] in
    case map scrub xos of
      [Just xo] -> 
          let xs' = delete xo xs in
          let xo' = putRevisionInfo ((getRevisionInfo xo) {nodeStatus = status}) xo in
          (putIxSet (insert xo' xs') store, xo')
      [Nothing] -> error "Permission denied"
      [] -> error ("Not found: " ++ show rev)
      xs -> error ("Duplicate revisions: " ++ show (map getRevisionInfo (catMaybes xs)))

-- |Delete a revision from the store, and remove its revision number
-- from all parent lists.  Return the new head, if there still is one.
-- Note the distinction between this and closing a revision, which
-- leaves it in the store but sets its status to NonHead without
-- creating any children.
deleteRev :: forall set elt s. (Store set elt s) =>
             (elt -> Maybe elt) -> Revision -> set -> Either (Maybe elt) (set, Maybe elt)
deleteRev scrub rev store =
    case map scrub xos of
      [] -> Left Nothing         -- Nothing to delete
      [Nothing] -> Left Nothing  -- Permission denied
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
          Right (putIxSet xs' store, Just xo)
      _ -> error "Conflict"
    where
      xs = getIxSet store :: IxSet elt
      xis = xs @= ident rev :: IxSet elt
      xos = (toList $ xis @= rev) :: [elt]      -- For each child of the victim node, replace the victim node's

-- |Prune (delete) any elements that aren't heads or a closest
-- common ancestor of some pair of heads.  Note that some revision
-- that is not a head may be sitting in a form in somebody's browser,
-- and when they edit and submit that form they could create a new
-- head which has ancestors this function would have deleted.  There
-- are several potential solutions to this problem, the simplest is to
-- implement two way merging.
prune :: forall set elt s. (Store set elt s) =>
         (elt -> Maybe elt) -> Ident -> set -> Maybe set
prune scrub i store =
    case discard of
      [] -> Nothing
      _ -> Just $ putIxSet (foldr delete set (trace ("  prune - discard=" ++ showRevs discard) discard)) store
    where
      discard :: [elt]
      discard = toList (foldr delete (set @= i) (trace ("  prune - keep=" ++ showRevs keep) keep))
      keep :: [elt]
      keep = catMaybes (heads ++ map original (catMaybes triplets))
      triplets :: [Maybe (Triplet elt)]
      triplets = askTriplets scrub i store
      heads = askHeads scrub i store
      set = getIxSet store

-- FIXME - make this a query so we don't have to convert to a list
{-
heads :: (Data a, Indexable a s, Revisable a, Ord a) => IxSet a -> [a]
heads s = toList (s @= Head)
-}
-- heads s = filter ((== Head) . nodeStatus . getRevisionInfo) . toList $ s

-- |Declare MERGED to be the child of PARENTS - allocate a new
-- revision, put it into MERGED's revision field with the list of
-- parents, set its nodeStatus to Head, and set the nodeStatus of all
-- the parents to NonHead.  Note that this can be used to create a
-- revision (by passing an empty parent list), revise a single item,
-- or merge several items.  It can also be used to create a branch 
-- by revising an element that already has children.
replace1 :: forall set elt s. (Store set elt s, Indexable elt s) => (elt -> Maybe elt) -> [Revision] -> elt -> set -> (set, elt)
replace1 scrub parentRevs merged store =
    case replace scrub parentRevs [merged] store of
      (store', [merged']) -> (store', merged')
      _ -> error "Unexpected result from replace"

-- |Replace zero or more parents with zero or more children.
replace :: forall set elt s. (Store set elt s, Indexable elt s) =>
           (elt -> Maybe elt) -> [Revision] -> [elt] -> set -> (set, [elt])
replace scrub parentRevs children store =
    case parentIds ++ childIds of
      [] -> error "replace: No parents and no children"
      ids@(i : _) | allEqual ids -> replace' scrub i parentRevs children store
      _ids -> error $ "replace: id mismatch: parentIds=" ++ show parentIds ++ ", childIds=" ++ show childIds
    where
      childIds = map (ident . revision . getRevisionInfo) children
      parentIds = map ident parentRevs

replace' :: forall set elt s. (Store set elt s, Indexable elt s) =>
            (elt -> Maybe elt) -> Ident -> [Revision] -> [elt] -> set -> (set, [elt])
replace' scrub i parentRevs children store =
    case any isNothing parents of
      True -> error "replace: Permission denied"
      False -> trace ("  replace': i=" ++ show i ++ ", parents=" ++ show parentRevs ++ ", children=" ++ show childRevs')
                 (store'', children')
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
      childInfo :: [RevisionInfo]
      childInfo = map (\ rev -> RevisionInfo {revision = rev,
                                              parentRevisions = map number parentRevs,
                                              nodeStatus = Head}) childRevs'
      childRevs' :: [Revision]
      childRevs' = map (\ (rev, n) -> rev {number = n + getMaxRev i store}) (zip childRevs [1..])
      childRevs :: [Revision]
      childRevs = map (revision . getRevisionInfo) children
      set :: IxSet elt
      set = getIxSet store
      unHead x xs = insert x' (delete x xs)
          where x' = putRevisionInfo (f (getRevisionInfo x)) x
                f :: RevisionInfo -> RevisionInfo
                f x = x {nodeStatus = NonHead}

-- |Close some revisions without creating any children.
close :: forall set elt s. (Store set elt s, Indexable elt s) => (elt -> Maybe elt) -> [Revision] -> set -> (set)
close scrub revs store = fst $ replace scrub revs [] store

-- Utility functions.

-- |Look for cases where the same revision exists in the database
-- twice, once with status Head and once with status NonHead.  This
-- should never happen, but it did once due to a bug, and this was
-- used to repair the database.
_fixBadRevs :: forall set elt s. (Store set elt s, Revisable elt) => Ident -> set -> set
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

showTriplet :: Revisable a => Triplet a -> String
showTriplet (Triplet o l r) = "Triplet {o=" ++ maybe "Nothing" (show . revision . getRevisionInfo) o ++
                              ", l=" ++ (show . revision . getRevisionInfo $ l) ++
                              ", r=" ++ (show . revision . getRevisionInfo $ r) ++ "}"

traceThis :: (a -> String) -> a -> a
traceThis f x = trace (f x) x

showRevs :: Revisable a => [a] -> String
showRevs xs = show (map (revision . getRevisionInfo) xs)

rr :: Revisable a => a -> Revision
rr = revision . getRevisionInfo

allEqual :: Eq a => [a] -> Bool
allEqual (x : more) = all (\ y -> x == y) more
allEqual [] = True

{-
allEqualBy :: Eq b => (a -> b) -> [a] -> Bool
allEqualBy f (x : more) = all (\ y -> f x == f y) more
allEqualBy _ [] = True
-}

_traceRev :: Revisable a => String -> a -> a
_traceRev prefix x = trace (prefix ++ show (getRevisionInfo x)) x
_traceRevs :: Revisable a => String -> [a] -> [a]
_traceRevs prefix xs = trace (prefix ++ show (map getRevisionInfo xs)) xs
