{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances,
             MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, TemplateHaskell,
             UndecidableInstances #-}
{-# OPTIONS -fno-warn-orphans -Wwarn #-}
module Happstack.Data.IxSet.Revision.Current
    ( Ident(..)
    , Revision(..)
    , RevisionInfo(..)
    , NodeStatus(..)
    ) where

import Data.Generics
import Happstack.Data (Default(..), deriveNewData, deriveNewDataNoDefault)
import Happstack.State (EpochMilli)

-- | Identifier for a item which can have multiple revisions.
newtype Ident
    = Ident {unIdent :: Integer}
    deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveNewData [''Ident])

data NodeStatus = Head | NonHead deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveNewData [''NodeStatus])

-- | Identifier for a particular revision of a particular item.
data Revision k
    = Revision {ident :: k, number :: Integer}
    deriving (Eq, Ord, Read, Data, Typeable)

$(deriveNewDataNoDefault [''Revision])

instance Default k => Default (Revision k) where
    defaultValue = Revision {ident = defaultValue, number = 1}

-- | The information associated with a revision to record its status.
data RevisionInfo k
    = RevisionInfo 
      { revision :: Revision k
      , created :: EpochMilli
      , parentRevisions :: [Integer]
      , nodeStatus :: NodeStatus}
    deriving (Eq, Ord, Read, Data, Typeable)

$(deriveNewDataNoDefault [''RevisionInfo])

instance Default k => Default (RevisionInfo k) where
    defaultValue = RevisionInfo {revision = defaultValue, created = 0, parentRevisions = [], nodeStatus = Head}

instance Show k => Show (RevisionInfo k) where
    show r = "(" ++ show (revision r) ++
             " created: " ++ show (created r) ++
             (if nodeStatus r == Head then " (Head)" else " (NonHead)") ++
             " parents: " ++ show (parentRevisions r) ++ ")"

instance Show k => Show (Revision k) where
    show r = show (ident r) ++ "." ++ show (number r)
