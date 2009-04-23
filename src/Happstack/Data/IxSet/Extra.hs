{-# LANGUAGE FlexibleContexts #-}
module Happstack.Data.IxSet.Extra 
    (testAndInsert
    )
        where

import Control.Monad.State
import Data.Generics
--import Happstack.Data
import Happstack.Data.IxSet

-- |perform insert only if test is True
testAndInsert :: (Indexable a b,
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