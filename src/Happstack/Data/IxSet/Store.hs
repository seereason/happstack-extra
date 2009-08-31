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
    , reviseElt
    , mergeElts
    , combineElts
    , deleteRev
    , setStatus
    , merge
    , fixBadRevs
    ) where

import Data.Data (Data)
import Data.Function (on)
import Data.List (tails, groupBy, sortBy)
import qualified Data.Map as M
import Data.Maybe (isJust, catMaybes)
import Happstack.Data (deriveSerialize, Default(..), deriveAll)
import Happstack.Data.IxSet (Indexable(..), IxSet(..), (@=), toList, delete, insert)
import Happstack.Data.IxSet.Merge (twoOrThreeWayMerge)
import Happstack.Data.IxSet.POSet (commonAncestor)
import Happstack.Data.IxSet.Revision (Revisable(getRevisionInfo, putRevisionInfo),
                                      RevisionInfo(RevisionInfo, revision, parentRevisions),
                                      Revision(ident, number), Ident(Ident), NodeStatus(Head, NonHead), nodeStatus)
import Happstack.State (Version)

import Debug.Trace

traceThis :: (a -> String) -> a -> a
traceThis f x = trace (f x) x

class (Revisable elt, Indexable elt (), Data elt, Ord elt) => Store set elt | set -> elt where
    getMaxId :: set -> Ident
    putMaxId :: Ident -> set -> set
    getMaxRevs :: set -> M.Map Ident Integer
    putMaxRevs :: M.Map Ident Integer -> set -> set
    getIxSet :: set -> IxSet elt
    putIxSet :: IxSet elt -> set -> set

-- FIXME - we need a safer way to increase and use the max rev, like getNextId
putMaxRev :: Store set elt => Ident -> Integer -> set -> set
putMaxRev ident rev s = putMaxRevs (M.insert ident rev (getMaxRevs s)) s

getMaxRev :: forall set elt. (Store set elt, Revisable elt) => Ident -> set -> Integer
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

getNextId :: (Store set elt) => set -> (set, Ident)
getNextId x = 
    (putMaxId newId x, newId)
    where
      newId = Ident (oldId + 1)
      (Ident oldId) = getMaxId x

askHeads :: (Store set elt) => (elt -> Maybe elt) -> Ident -> set -> [Maybe elt]
askHeads scrub i store =
    let xis = (getIxSet store) @= Head @= (trace ("  askHeads " ++ show i) i) in
    case map scrub (toList xis) of
      [] -> error $ "askHeads - no head: " ++ show (map getRevisionInfo (toList xis))
      xs -> trace ("  askHeads -> " ++ show (map (fmap getRevisionInfo) xs)) xs

askAllRevs :: (Store set elt, Revisable elt) => (elt -> Maybe elt) -> Ident -> set -> [Maybe elt]
askAllRevs scrub i store =
    let xis = (getIxSet store) @= i in
    case map scrub (toList xis) of
      [] -> error $ "askAllRevs - no head: " ++ show (map getRevisionInfo (toList xis))
      xs -> trace ("  askAllRevs -> " ++ show (map (fmap getRevisionInfo) xs)) xs

askRev :: (Store set elt) => (elt -> Maybe elt) -> Revision -> set -> Maybe elt
askRev scrub rev store =
    case map scrub (toList (getIxSet store @= (trace ("  askRev " ++ show rev) rev))) of
      [] -> trace "askRev -> Nothing" Nothing
      [Just x] -> Just (trace ("  askRev -> " ++ show (getRevisionInfo x)) x)
      [Nothing] -> error "askRev: permission denied"
      xs -> error ("askRev: duplicate revisions: " ++ show (map getRevisionInfo (catMaybes xs)))

askHeadTriplets :: (Store set elt) => (elt -> Maybe elt) -> Ident -> set -> [Maybe (Triplet elt)]
askHeadTriplets scrub i store =
    let xis = (getIxSet store) @= i in
    case toList (xis @= Head) of
      [] -> []
      rs -> triples (\ x y -> commonAncestor xis x y) rs
    where
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


askAllHeads :: (Store set elt) => (elt -> Maybe elt) -> set -> [Maybe elt]
askAllHeads scrub = map scrub . heads . getIxSet

-- |Create a new revision of an existing element, making the new
-- |revision the parent and a head.
reviseElt :: (Store set elt) => (elt -> Maybe elt) -> elt -> set -> Either elt (set, elt)
reviseElt scrub x store =
    case map scrub (toList (set @= oldRev)) of
      [Just x0] ->
          if x == putRevisionInfo (getRevisionInfo x) x0
          then Left (trace " -> No revision necessary" x0)
          else let (store', x') = merge store [x0] x in
               Right (store', (trace (" -> " ++ show (getRevisionInfo x')) x'))
      [Nothing] -> error (traceString "reviseElt: permission denied")
      [] ->        error (traceString ("reviseElt: Not found: " ++ show oldRev))
      xs ->         error (traceString ("reviseElt: duplicate revision: " ++ show (map getRevisionInfo (catMaybes xs))))
    where
      oldRev = revision . getRevisionInfo $ x
      set = getIxSet store

traceString :: String -> String
traceString s = trace s s

-- |Create a new revision which is the child of several existing
-- revisions.  In other words, merge several heads into one.  The
-- revision number in x is ignored, but the Ident must match the
-- parent elements.
mergeElts :: (Store set elt) => (elt -> Maybe elt) -> [elt] -> elt -> set -> (set, elt)
mergeElts scrub parents x store =
    let i = ident (revision (getRevisionInfo x)) in
    if any (/= i) (map (ident . revision . getRevisionInfo) parents)
    then error "Parent idents don't match merged element"
    else if all isJust (map scrub parents)
         then traceThis (\ (_, m) -> "  mergeElts " ++ show (map getRevisionInfo parents) ++ " -> " ++ show (getRevisionInfo m))
                  (merge store parents x)
         else error "Insuffient permissions"

-- Examine the set of head revisions and attempt to merge as many as
-- possible using the automatic threeWayMerge function.
combineElts :: forall set elt. (Store set elt) => (elt -> Maybe elt) -> (elt -> Maybe elt) -> Ident -> set -> Either [elt] (set, [elt])
combineElts readScrub writeScrub i set =
    merge False set (askHeadTriplets readScrub i set)
    where
      -- No triplets to merge, 
      merge :: Bool -> set -> [Maybe (Triplet elt)] -> Either [elt] (set, [elt])
      merge merged set [] =
          let heads = toList ((getIxSet set @= i) @= Head) in
          if merged then Right (set, heads) else Left heads
      -- Try to merge each of the triplets in turn
      merge merged set (Just (Triplet o l r) : more) =
          let copyRev s d = putRevisionInfo (getRevisionInfo s) d
              o' = maybe Nothing (Just . copyRev l) o
              r' = copyRev l r in
          case traceThis (\ m -> "  threeWayMerge " ++ show [fmap getRevisionInfo o, Just (getRevisionInfo l), Just (getRevisionInfo r)] ++ " -> " ++ show (fmap getRevisionInfo m))
                   (twoOrThreeWayMerge o' l r') of
            -- We merged a triplet, set the merged flag and re-start the combine process
            Just m -> let (set', _) = mergeElts writeScrub [l, r] m set in
                      merge True set' (askHeadTriplets readScrub i set)
            -- We couldn't merge a triplet, try the next
            Nothing -> merge merged set more
      -- Permission failure
      merge merged set (Nothing : more) = merge merged set more

_traceRev :: Revisable a => String -> a -> a
_traceRev prefix x = trace (prefix ++ show (getRevisionInfo x)) x
_traceRevs :: Revisable a => String -> [a] -> [a]
_traceRevs prefix xs = trace (prefix ++ show (map getRevisionInfo xs)) xs

setStatus :: forall set elt. (Store set elt) => (elt -> Maybe elt) -> NodeStatus -> Revision -> set -> (set, elt)
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

-- Delete the revision from the store, and anywhere it appears in an
-- element's parent list replace with its parent list.  Return the new
-- head, if there still is one.
deleteRev :: forall set elt. (Store set elt) => (elt -> Maybe elt) -> Revision -> set -> Either (Maybe elt) (set, Maybe elt)
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
                                      then replace x (setParentRevisions (filter (/= (number' xo)) (parentRevisions' x) ++ parentRevisions' xo) x) xs
                                      else xs in
          Right (putIxSet xs' store, Just xo)
      _ -> error "Conflict"
    where
      xs = getIxSet store :: IxSet elt
      xis = xs @= ident rev :: IxSet elt
      xos = (toList $ xis @= rev) :: [elt]      -- For each child of the victim node, replace the victim node's

-- FIXME - make this a query so we don't have to convert to a list
heads :: (Data a, Indexable a (), Revisable a, Ord a) => IxSet a -> [a]
heads s = toList (s @= Head)
-- heads s = filter ((== Head) . nodeStatus . getRevisionInfo) . toList $ s

-- |Insert a revision into the index and designate it the merger of a
-- list of existing revisions.  The status of the new revision will be
-- set to Head, the status of the parents set to NonHead.  Note that
-- this can be used to create a revision (by passing an empty parent
-- list), revise a single item, or merge several items.
merge :: forall set elt. (Store set elt, Indexable elt ()) => set -> [elt] -> elt -> (set, elt)
merge store parents merged =
    if any (/= i) parentIds
    then error $ "merge: ident mismatch: merged=" ++ show i ++ ", parents=" ++ show parentIds
    else (store', trace ("  merge: merged'=" ++ show (getRevisionInfo merged')) merged')
    where
      store' = putMaxRev i (number rev') (putIxSet set' store)
      set' = insert merged' (foldr unHead set parents)
      set = getIxSet store
      merged' = putRevisionInfo info' merged
      info' = RevisionInfo {revision = rev', parentRevisions = map number parentRevs, nodeStatus = Head}
      rev' = rev {number = 1 + getMaxRev i store}
      parentIds = map ident parentRevs
      parentRevs = map (revision . getRevisionInfo) parents
      i = ident rev
      rev = revision . getRevisionInfo $ merged
      unHead :: elt -> IxSet elt -> IxSet elt
      unHead x xs = insert x' (delete x xs)
          where x' = putRevisionInfo (f (getRevisionInfo x)) x
                f :: RevisionInfo -> RevisionInfo
                f x = x {nodeStatus = NonHead}

fixBadRevs :: forall set elt. (Store set elt, Revisable elt) => Ident -> set -> set
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
