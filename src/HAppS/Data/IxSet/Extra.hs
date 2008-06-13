{-# LANGUAGE FlexibleContexts #-}
module HAppS.Data.IxSet.Extra 
    (testAndInsert
    )
        where

import Control.Monad.State
import Data.Generics
import HAppS.Data
import HAppS.Data.IxSet

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
