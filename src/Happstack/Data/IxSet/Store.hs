{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell, UndecidableInstances #-}
-- |Abstracted database queries and updates on IxSets of Revisable elements.
module Happstack.Data.IxSet.Store
    ( Store(..)
    , getMaxRev
    , putMaxRev
    , Triplet(..)
    , getNextId
    , askHeads
    , askRev
    , askAllRevs
    , askHeadTriplets
    , askAllHeads
    --, reviseElt
    , reviseAndMerge
    --, replaceElts
    , combineHeads
    , deleteRev
    , setStatus
    , replace
    , replace1
    , close
    , fixBadRevs
    ) where

import Data.Data (Data)
import Data.Function (on)
import Data.List (tails, groupBy, sortBy)
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

traceThis :: (a -> String) -> a -> a
traceThis f x = trace (f x) x

class (Revisable elt, Indexable elt s, Data elt, Ord elt) => Store set elt s | set -> elt, set -> s where
    getMaxId :: set -> Ident
    putMaxId :: Ident -> set -> set
    getMaxRevs :: set -> M.Map Ident Integer
    putMaxRevs :: M.Map Ident Integer -> set -> set
    getIxSet :: set -> IxSet elt
    putIxSet :: IxSet elt -> set -> set

-- FIXME - we need a safer way to increase and use the max rev, like getNextId
putMaxRev :: Store set elt s => Ident -> Integer -> set -> set
putMaxRev ident rev s = putMaxRevs (M.insert ident rev (getMaxRevs s)) s

getMaxRev :: forall set elt s. (Store set elt s, Revisable elt) => Ident -> set -> Integer
getMaxRev ident s = 
    maybe getMaxRev' id (M.lookup ident (getMaxRevs s))
    where
      -- Look at all revisions - could be expensive
      getMaxRev' :: Integer
      getMaxRev' = foldr f 0 (toList (getIxSet s @= ident))
      f :: elt -> Integer -> Integer
      f x rev = max rev (number . revision . getRevisionInfo $ x)

$(deriveAll [''Eq, ''Ord, ''Read, ''Show]
  [d|
      -- data Triplet a = Triplet {original :: a, left :: a, right :: a}
      data Triplet a = Triplet {original :: Maybe a, left :: a, right :: a}
      -- data Triplet a = Triplet {original :: Maybe a, left :: Maybe a, right :: Maybe a}
   |])

instance (Ord a, Default a) => Default (Triplet a) where
    defaultValue = Triplet defaultValue defaultValue defaultValue

$(deriveSerialize ''Triplet)
instance Version (Triplet a)

getNextId :: (Store set elt s) => set -> (set, Ident)
getNextId x = 
    (putMaxId newId x, newId)
    where
      newId = Ident (oldId + 1)
      (Ident oldId) = getMaxId x

askHeads :: (Store set elt s) => (elt -> Maybe elt) -> Ident -> set -> [Maybe elt]
askHeads scrub i store =
    let xis = (getIxSet store) @= Head @= (trace ("  askHeads " ++ show i) i) in
    case map scrub (toList xis) of
      [] -> error $ "askHeads - no head: " ++ show (map getRevisionInfo (toList xis))
      xs -> trace ("  askHeads -> " ++ show (map (fmap getRevisionInfo) xs)) xs

askAllRevs :: (Store set elt s, Revisable elt) => (elt -> Maybe elt) -> Ident -> set -> [Maybe elt]
askAllRevs scrub i store =
    let xis = (getIxSet store) @= i in
    case map scrub (toList xis) of
      [] -> error $ "askAllRevs - no head: " ++ show (map getRevisionInfo (toList xis))
      xs -> trace ("  askAllRevs -> " ++ show (map (fmap getRevisionInfo) xs)) xs

askRev :: (Store set elt s) => (elt -> Maybe elt) -> Revision -> set -> Maybe elt
askRev scrub rev store =
    case map scrub (toList (getIxSet store @= (trace ("  askRev " ++ show rev) rev))) of
      [] -> trace "askRev -> Nothing" Nothing
      [Just x] -> Just (trace ("  askRev -> " ++ show (getRevisionInfo x)) x)
      [Nothing] -> error "askRev: permission denied"
      xs -> error ("askRev: duplicate revisions: " ++ show (map getRevisionInfo (catMaybes xs)))

-- |Return an item's list of (original, left, right) triplets - the
-- list of pairs of head elements, with the common ancestor.
askHeadTriplets :: (Store set elt s) => (elt -> Maybe elt) -> Ident -> set -> [Maybe (Triplet elt)]
askHeadTriplets scrub i store =
    triples (commonAncestor xis) heads
    where
      heads = toList (xis @= Head)
      -- This is going to be slow if there are a lot of revisions, but
      -- it is required by the commonAncestor function.  This is why
      -- the revision set needs to be pruned.
      xis = (getIxSet store) @= i
      -- Build the list of triples.
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


askAllHeads :: (Store set elt s) => (elt -> Maybe elt) -> set -> [Maybe elt]
askAllHeads scrub = map scrub . heads . getIxSet

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
    merge False set (traceThis (\ triplets -> "  combineHeads: length triplets=" ++ show (length triplets))
                     (askHeadTriplets scrub (trace ("  combineHeads " ++ show i)
                                             i) set))
    where
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
                      merge True set' (askHeadTriplets scrub i set)
            -- We couldn't merge a triplet, try the next
            Nothing -> merge merged set more
      -- Permission failure
      merge _ _ (Just (Triplet _ l r) : _) =
          error ("combineHeads: missing ancestor of " ++ show [getRevisionInfo l, getRevisionInfo r])
      merge merged set (Nothing : more) = merge merged set more

_traceRev :: Revisable a => String -> a -> a
_traceRev prefix x = trace (prefix ++ show (getRevisionInfo x)) x
_traceRevs :: Revisable a => String -> [a] -> [a]
_traceRevs prefix xs = trace (prefix ++ show (map getRevisionInfo xs)) xs

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

-- FIXME - make this a query so we don't have to convert to a list
heads :: (Data a, Indexable a s, Revisable a, Ord a) => IxSet a -> [a]
heads s = toList (s @= Head)
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

allEqual :: Eq a => [a] -> Bool
allEqual (x : more) = all (\ y -> x == y) more
allEqual [] = True

allEqualBy :: Eq b => (a -> b) -> [a] -> Bool
allEqualBy f (x : more) = all (\ y -> f x == f y) more
allEqualBy _ [] = True

-- |Replace zero or more parents with zero or more children.
replace :: forall set elt s. (Store set elt s, Indexable elt s) =>
           (elt -> Maybe elt) -> [Revision] -> [elt] -> set -> (set, [elt])
replace scrub parentRevs children store =
    case parentIds ++ childIds of
      [] -> error "replace: No parents and no children"
      ids@(i : _) | allEqual ids -> replace' scrub i parentRevs children store
      ids -> error $ "replace: id mismatch: parentIds=" ++ show parentIds ++ ", childIds=" ++ show childIds
    where
      childIds = map (ident . revision . getRevisionInfo) children
      parentIds = map ident parentRevs

replace' :: forall set elt s. (Store set elt s, Indexable elt s) =>
            (elt -> Maybe elt) -> Ident -> [Revision] -> [elt] -> set -> (set, [elt])
replace' scrub i parentRevs children store =
    case any isNothing parents of
      True -> error "replace: Permission denied"
      False -> (store'', children')
    where
      store'' :: set
      store'' = putMaxRev i (getMaxRev i store' + toInteger (length children)) store'
      store' :: set
      store' = putIxSet set'' store'
      set'' :: IxSet elt
      set'' = foldr insert set' children'
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

fixBadRevs :: forall set elt s. (Store set elt s, Revisable elt) => Ident -> set -> set
fixBadRevs i store =
    foldr repair store (concat bad)
    where 
      repair x store =
          let rev = 1 + getMaxRev i store in
          let store' = putMaxRev i rev store in
          let info = getRevisionInfo x in
          let info' = info {revision = (revision info) {number = rev}, nodeStatus = Head} in
          let x' = putRevisionInfo (trace ("  Changing revision from " ++ show info ++ " to " ++ show info') info') x in
          let ix = getIxSet store' in
          let ix' = insert x' (delete x ix) in
          let store'' = putIxSet ix' store' in
          store''
      bad = filter (\ g -> length g > 1) 
                (groupBy ((==) `on` (number . revision . getRevisionInfo))
                 (sortBy (compare `on` (number . revision . getRevisionInfo)) revs))
      revs = toList (set @= i)
      set = getIxSet store
