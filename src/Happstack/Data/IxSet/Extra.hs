{-# LANGUAGE FlexibleContexts #-}
module Happstack.Data.IxSet.Extra 
    ( testAndInsert
    , difference
    )
        where

import Control.Monad.State
import Data.Generics
import qualified Data.Set as Set
--import Happstack.Data
import Happstack.Data.IxSet

-- |perform insert only if test is True
testAndInsert :: (Indexable a,
                  Ord a,
                  Data a,
                  MonadState (IxSet a) m) =>
                 (IxSet a -> Bool) -> a -> m Bool
testAndInsert test a =
    maybeModify $ \ixset ->
        if test ixset
          then Just (insert a ixset)
          else Nothing

-- this should be sent upstream to mtl
maybeModify :: (MonadState s m) => (s -> Maybe s) -> m Bool
maybeModify f =
    do state <- get
       case f state of
         Nothing -> return False
         (Just state') -> 
             do put state' 
                return True

-- * this should go in Data.Map
{-
insertIfNew :: (Ord k) => k -> a -> M.Map k a -> Maybe (M.Map k a)
insertIfNew k v m =
    case M.insertLookupWithKey (\_ _ oldValue -> oldValue) k v m of
      (Nothing, m') -> Just m'
      (Just _, _) -> return m
-}


-- | Takes the intersection of the two IxSets
difference :: (Ord a, Data a, Indexable a) => IxSet a -> IxSet a -> IxSet a
difference x1 x2 = fromSet $ Set.difference (toSet x1) (toSet x2)
