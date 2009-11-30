{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, TemplateHaskell #-}
{-# OPTIONS -fno-warn-orphans #-}
module Happstack.Data.IxSet.Revision.Instances where

import Data.Generics
import Happstack.Data (Version(..), Proxy(..), Mode(..), proxy, extension, deriveSerialize, Migrate(..))
import qualified Happstack.Data.IxSet.Revision.Old1 as O1
import qualified Happstack.Data.IxSet.Revision.Current as C


---------------
-- MIGRATION --
---------------

-- |Obsolete version of the RevisionInfo structure.  The isHead field
-- was changed to nodeStatus :: NodeStatus.
data RevisionInfo001
    = RevisionInfo001 {revision001 :: C.Revision, parentRevisions001 :: [Integer], isHead001 :: Bool}
    deriving (Eq, Ord, Read, Show, Data, Typeable)

instance Migrate RevisionInfo001 O1.RevisionInfo where
    migrate r =
        O1.RevisionInfo { O1.revision = revision001 r
                        , O1.parentRevisions = parentRevisions001 r
                        , O1.nodeStatus = if isHead001 r then C.Head else C.NonHead }

instance Version RevisionInfo001
$(deriveSerialize ''RevisionInfo001)
instance Version O1.RevisionInfo where mode = extension 1 (proxy undefined :: Proxy RevisionInfo001) :: Mode O1.RevisionInfo
$(deriveSerialize ''O1.RevisionInfo)
instance Version C.Revision
$(deriveSerialize ''C.Revision)
instance Version C.NodeStatus
$(deriveSerialize ''C.NodeStatus)
instance Version C.Ident
$(deriveSerialize ''C.Ident)


$(deriveSerialize ''C.RevisionInfo)

instance Version C.RevisionInfo where
    mode = extension 2 (Proxy :: Proxy O1.RevisionInfo) 

instance Migrate O1.RevisionInfo C.RevisionInfo where
    migrate r =
        C.RevisionInfo { C.revision        = O1.revision r
                       , C.created         = 0
                       , C.parentRevisions = O1.parentRevisions r
                       , C.nodeStatus      = O1.nodeStatus r
                       }