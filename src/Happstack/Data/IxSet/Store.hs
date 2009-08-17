{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell, UndecidableInstances #-}
-- |Abstracted database queries and updates on IxSets of Revisable elements.
module Happstack.Data.IxSet.Store
    ( Store(..)
    , Triplet(..)
    , getNextId
    , askHeads
    , askRev
    , askHeadTriplets
    , askAllHeads
    , reviseElt
    , deleteRev
    ) where

import Data.Data (Data)
import Data.List (tails)
import Happstack.Data (deriveSerialize, Default(..), deriveAll)
import Happstack.Data.IxSet (Indexable(..), IxSet(..), (@=), toList, delete, insert)
import Happstack.Data.IxSet.POSet (commonAncestor)
import Happstack.Data.IxSet.Revision (revise, merge, Revisable(getRevisionInfo, putRevisionInfo),
                                      RevisionInfo(revision, parentRevisions), Revision(ident, number), Ident(Ident), NodeStatus(Head), nodeStatus)
import Happstack.State (Version)

class (Revisable elt, Indexable elt (), Data elt, Ord elt) => Store set elt | set -> elt where
    getMaxId :: set -> Ident
    putMaxId :: Ident -> set -> set
    getIxSet :: set -> IxSet elt
    putIxSet :: IxSet elt -> set -> set

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
    let xis = (getIxSet store) @= i in
    case map scrub (heads xis) of
      [] -> error $ "AskHeads - no head: " ++ show (map getRevisionInfo (toList xis))
      xs -> xs

askRev :: (Store set elt) => (elt -> Maybe elt) -> Revision -> set -> Maybe elt
askRev scrub rev store =
    case map scrub (toList (getIxSet store @= rev)) of
      [] -> Nothing
      [Just x] -> Just x
      [Nothing] -> error "permission denied"
      _ -> error ("duplicate revisions: " ++ show rev)

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

reviseElt :: (Store set elt) => (elt -> Maybe elt) -> elt -> set -> Either elt (set, elt)
reviseElt scrub x store =
    let xs = getIxSet store
        rev = revision (getRevisionInfo x)
        xis = xs @= ident rev
        (xs', x') = revise xs xis x in
    case map scrub (toList (xis @= rev)) of 
      [Just xo] ->
          if x' == putRevisionInfo (getRevisionInfo x') xo
          then Left xo
          else Right (putIxSet xs' store, x')
      [Nothing] ->
          error "permission denied"
      [] -> error "not found"
      _ -> error ("duplicate revision: " ++ show rev)

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
