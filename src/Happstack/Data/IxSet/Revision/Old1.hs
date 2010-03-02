{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, UndecidableInstances #-}
module Happstack.Data.IxSet.Revision.Old1 where

import Data.Generics
import Happstack.Data
import Happstack.Data.IxSet.Revision.Current (Ident, Revision, NodeStatus)

-- | The information associated with a revision to record its status.
data RevisionInfo
    = RevisionInfo {revision :: Revision Ident, parentRevisions :: [Integer], nodeStatus :: NodeStatus}
    deriving (Eq, Ord, Read, Data, Typeable)

$(deriveNewDataNoDefault [''RevisionInfo])