{-# LANGUAGE FlexibleContexts, FunctionalDependencies, MultiParamTypeClasses, ScopedTypeVariables #-}
-- |Abstracted database queries and updates on IxSets of Revisable elements.
module Happstack.Data.IxSet.Store
    ( Store(..)
    , getNextId
    , askHeads
    , askRev
    , askAllHeads
    , reviseElt
    , deleteRev
    ) where

import Data.Data (Data)
import Happstack.Data.IxSet (Indexable(..), IxSet(..), (@=), toList, delete, insert)
import Happstack.Data.IxSet.Revision (revise, Revisable(getRevisionInfo, putRevisionInfo),
                                      RevisionInfo(revision, parentRevisions), Revision(ident, number), Ident(Ident), isHead)

class (Revisable elt, Indexable elt (), Data elt, Ord elt) => Store set elt | set -> elt where
    getMaxId :: set -> Ident
    putMaxId :: Ident -> set -> set
    getIxSet :: set -> IxSet elt
    putIxSet :: IxSet elt -> set -> set

getNextId :: (Store set elt) => set -> (set, Ident)
getNextId x = 
    (putMaxId newId x, newId)
    where
      newId = Ident (oldId + 1)
      (Ident oldId) = getMaxId x

askHeads :: (Store set elt) => Ident -> set -> [elt]
askHeads i store =
    let xis = (getIxSet store) @= i in
    case heads xis of
      [] -> error $ "AskHeads - no head: " ++ show (map getRevisionInfo (toList xis))
      xs -> xs

askRev :: (Store set elt) => Revision -> set -> Maybe elt
askRev rev store =
    case toList (getIxSet store @= rev) of
      [] -> Nothing
      [x] -> Just x
      -- This really shouldn't happen
      _ -> error ("AskRev - duplicate revisions: " ++ show rev)

askAllHeads :: (Store set elt) => set -> [elt]
askAllHeads = heads . getIxSet

reviseElt :: (Store set elt) => elt -> set -> Either elt (set, elt)
reviseElt x store =
    let xs = getIxSet store
        rev = revision (getRevisionInfo x)
        xis = xs @= ident rev
        xo = xis @= rev
        (xs', x') = revise xs xis x in
    case [x'] == map (putRevisionInfo (getRevisionInfo x')) (toList xo) of
      True -> Left x
      False -> Right (putIxSet xs' store, x')

-- Delete the revision from the store, and anywhere it appears in an
-- element's parent list replace with its parent list.  Return the new
-- head, if there still is one.
deleteRev :: forall set elt. (Store set elt) => Revision -> set -> Either (Maybe elt) (set, Maybe elt)
deleteRev rev store =
    case xos of
      [] -> Left Nothing
      [xo] -> 
          let number' = number . revision . getRevisionInfo
              parentRevisions' = parentRevisions . getRevisionInfo
              setParentRevisions revs x = putRevisionInfo ((getRevisionInfo x) {parentRevisions = revs}) x
              isHead' = isHead . getRevisionInfo
              setHead flag x = putRevisionInfo ((getRevisionInfo x) {isHead = flag}) x
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
                                 then replace x (setHead True x) xs
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
heads :: (Revisable a, Ord a) => IxSet a -> [a]
heads s = filter (isHead . getRevisionInfo) . toList $ s
