{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}
{-# OPTIONS -fno-warn-orphans #-}
module Happstack.Data.IxSet.Revision.Instances where

import Data.Generics
import Happstack.Data (Version(..), Proxy(..), Mode(..), proxy, extension, deriveSerialize, Migrate(..))
-- import qualified Happstack.Data.IxSet.Revision.Old1 as O1
import qualified Happstack.Data.IxSet.Revision.Current as C


---------------
-- MIGRATION --
---------------

-- |Obsolete version of the RevisionInfo structure.  The isHead field
-- was changed to nodeStatus :: NodeStatus.
data RevisionInfo001
    = RevisionInfo001
      { revision001 :: C.Revision C.Ident
      , parentRevisions001 :: [Integer]
      , isHead001 :: Bool }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

instance Version (C.Revision k)
$(deriveSerialize ''C.Revision)
instance Version C.NodeStatus
$(deriveSerialize ''C.NodeStatus)
instance Version C.Ident
$(deriveSerialize ''C.Ident)


$(deriveSerialize ''C.RevisionInfo)

instance Version (C.RevisionInfo k)
