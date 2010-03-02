{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, UndecidableInstances #-}
module Happstack.Data.IxSet.Revision.Old1 where

import Data.Generics
import Happstack.Data
import Happstack.Data.IxSet.Revision.Current (Ident, NodeStatus)
import Happstack.State (EpochMilli)

-- | The information associated with a revision to record its status.
data RevisionInfo001
    = RevisionInfo001 
      { revision001 :: Revision002
      , parentRevisions001 :: [Integer]
      , nodeStatus001 :: NodeStatus
      } deriving (Eq, Ord, Read, Show, Data, Typeable)

data Revision002
    = Revision002 
      { ident002 :: Ident
      , number002 :: Integer
      } deriving (Eq, Ord, Read, Show, Data, Typeable)

data RevisionInfo002
    = RevisionInfo002
      { revision002 :: Revision002
      , created002 :: EpochMilli
      , parentRevisions002 :: [Integer]
      , nodeStatus002 :: NodeStatus
      } deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveNewDataNoDefault [''RevisionInfo001, ''RevisionInfo002, ''Revision002])
