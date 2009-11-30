{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances,
             MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, TemplateHaskell,
             UndecidableInstances #-}
{-# OPTIONS -fno-warn-orphans -Wwarn #-}
module Happstack.Data.IxSet.Revision.Current
    ( Ident(..)
    , Revision(..)
    , RevisionInfo(..)
--    , changeRevisionInfo
--    , RevisionInfo001(..)
--    , Revisable(..)
    , NodeStatus(..)
--    , copyRev
--    , initialRevision
    -- , merge
    -- , revise
{-
    , Happstack.Data.IxSet.Revision.Current.prune
    , Heads
    , heads
    -- , combine
    , combineInfo
    , showRev
    , combine3
    , combine3M
    , conflicts
    , eqEx
-}
    ) where

--import qualified Data.ByteString.Char8 as B
import Data.Generics
--import Data.List (tails, intercalate)
--import qualified Data.Set as S
import Happstack.Data (Default(..), deriveNewData, deriveNewDataNoDefault)
--import Happstack.Data.IxSet
--import Happstack.Data.IxSet.POSet
--import qualified Happstack.Data.IxSet.POSet as P
--import Happstack.Data.IxSet.Triplets (mergeBy, mergeByM, mkQ2, extQ2, gzipBut3)
import Happstack.State (EpochMilli)


--import Debug.Trace

-- We need newtypes for each of these so we can make them IxSet
-- indexes.  That is also why they must each be a separate field
-- of the revisioned type.

-- | Identifier for a item which can have multiple revisions.
newtype Ident
    = Ident {unIdent :: Integer}
    deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveNewData [''Ident])

data NodeStatus = Head | NonHead deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveNewData [''NodeStatus])

-- | Identifier for a particular revision of a particular item.
data Revision
    = Revision {ident :: Ident, number :: Integer}
    deriving (Eq, Ord, Read, Data, Typeable)

$(deriveNewDataNoDefault [''Revision])

instance Default Revision where
    defaultValue = Revision {ident = defaultValue, number = 1}

-- | The information associated with a revision to record its status.
data RevisionInfo
    = RevisionInfo {revision :: Revision, created :: EpochMilli , parentRevisions :: [Integer], nodeStatus :: NodeStatus}
    deriving (Eq, Ord, Read, Data, Typeable)

$(deriveNewDataNoDefault [''RevisionInfo])

instance Default RevisionInfo where
    defaultValue = RevisionInfo {revision = defaultValue, created = 0, parentRevisions = [], nodeStatus = Head}

instance Show RevisionInfo where
    show r = "(" ++ show (revision r) ++
             " created: " ++ show (created r) ++
             (if nodeStatus r == Head then " (Head)" else " (NonHead)") ++
             " parents: " ++ show (parentRevisions r) ++ ")"

instance Show Revision where
    show r = show (unIdent (ident r)) ++ "." ++ show (number r)
