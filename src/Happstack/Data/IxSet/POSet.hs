{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-- |A class for partially ordered sets, or lattices.  This is used by
-- the Revision module, where merging revisions creates nodes with
-- multiple parents, and forking them creates nodes with multiple
-- children, but thankfully there is no way I can think of to create a
-- cycle.
module Happstack.Data.IxSet.POSet 
    ( POSet(..)
    , commonAncestor
    , commonAncestors
    , prune
    ) where
    
import Data.List (partition)
import Data.Maybe (catMaybes)
import qualified Data.Set as S

-- |Class of partially ordered sets.  This is a graph with no cycles,
-- but not a tree because a child may have more than one parent.
-- Another definition is a relation which is reflexive,
-- anti-symmetric, and transitive.  The non-cyclic requirement is not
-- enforced, but it can be verified and the algorithms assume it.
class (Eq a, Ord a) => POSet s a where
    parents :: s -> a -> [a]

-- |Every pair of elements has a single common ancestor, possibly one
-- of the arguments.
commonAncestor :: POSet s a => s -> a -> a -> Maybe a
commonAncestor s x y =
    -- Add parents to family until we find a common element
    f (S.singleton x) (S.singleton x) (S.singleton y) (S.singleton y)
    where
      f xFamily xEldest yFamily yEldest =
          let i = S.intersection xFamily yFamily in
          if S.null xEldest && S.null yEldest
          then Nothing
          else case S.toList i of
                 [] ->
                     let -- Compute the new eldest sets - find all the parents of
                         -- the current eldest set, then subtract anything already
                         -- in the family.
                         xEldest' = S.difference (S.fromList (concatMap (parents s) (S.toList xEldest))) xFamily
                         yEldest' = S.difference (S.fromList (concatMap (parents s) (S.toList yEldest))) yFamily
                         -- Add the new eldest set to the current family
                         xFamily' = S.union xFamily xEldest'
                         yFamily' = S.union yFamily yEldest' in
                     f xFamily' xEldest' yFamily' yEldest'
                 -- Found one, return it.  Maybe there are more than
                 -- one, but we can do a three way merge with any of
                 -- them.
                 (ancestor : _) -> Just ancestor
          
    -- find (`elem` (ancestors s y)) (ancestors s x)

-- |Find the set containing the common ancestor of every pair in xs.
-- The result will be a superset of the input list.  I'm implementing
-- this to garbage collect a simple revision system where we only care
-- about preserving the heads and their common ancestors in order to
-- do 3-way merges.
commonAncestors :: POSet s a => s -> [a] -> S.Set a
commonAncestors _ [] = S.empty
commonAncestors _ [x] = S.singleton x
commonAncestors s (x : xs) = 
    S.union (S.fromList (x : catMaybes (map (\ y -> commonAncestor s x y) xs))) (commonAncestors s xs)
    

-- |Given a set nodes to keep, return modified parent lists for each
-- of those nodes, along with a list of nodes to delete.  If the set
-- of nodes to keep is the result of passing the list of heads to
-- commonAncestors, this function will compute the information
-- required to remove all the nodes that are not the closest common
-- ancestors of two kept nodes.
prune :: POSet s a => s -> S.Set a -> ([(a, [a])], S.Set a)
prune s keep =
    (zip (S.toList keep) parentLists, S.unions victimSets)
    where
      (parentLists, victimSets) =
          unzip (map (\x -> reparent S.empty (parents s x)) (S.toList keep))
      reparent victims xs =
          case partition (`S.member` keep) xs of
            (ok, []) -> (ok, victims)
            (ok, bad) -> reparent (S.union victims (S.fromList bad))
                                              (ok ++ concatMap (parents s) bad)
