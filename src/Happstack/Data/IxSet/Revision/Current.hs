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
import qualified Data.Generics.SYB.WithClass.Basics as N
import qualified Data.Generics.SYB.WithClass.Context as N
import Happstack.Data (Default(..), deriveNewData, deriveNewDataNoDefault, deriveAll)
import Happstack.State (EpochMilli)

-- | Identifier for a item which can have multiple revisions.
newtype Ident
    = Ident {unIdent :: Integer}
    deriving (Eq, Ord, Read, Show, Data, Typeable)

instance Enum Ident where
    toEnum = Ident . toInteger
    fromEnum = fromInteger . unIdent

data NodeStatus = Head | NonHead deriving (Eq, Ord, Read, Show, Data, Typeable)

-- | Identifier for a particular revision of a particular item.
data Enum k => Revision k
    = Revision {ident :: k, number :: Integer}
    deriving (Eq, Ord, Read, Data, Typeable)

-- | The information associated with a revision to record its status.
data Enum k => RevisionInfo k
    = RevisionInfo 
      { revision :: Revision k
      , created :: EpochMilli
      , parentRevisions :: [Integer]
      , nodeStatus :: NodeStatus}
    deriving (Eq, Ord, Read, Data, Typeable)

instance (Enum k, Show k) => Show (RevisionInfo k) where
    show r = "(" ++ show (revision r) ++
             " created: " ++ show (created r) ++
             (if nodeStatus r == Head then " (Head)" else " (NonHead)") ++
             " parents: " ++ show (parentRevisions r) ++ ")"

instance (Enum k, Show k) => Show (Revision k) where
    show r = show (ident r) ++ "." ++ show (number r)

$(deriveNewData [''Ident, ''NodeStatus])

instance (Enum k, Default k) => Default (Revision k) where
    defaultValue = Revision {ident = defaultValue, number = 1}

instance (Enum k, Default k) => Default (RevisionInfo k) where
    defaultValue = RevisionInfo {revision = defaultValue, created = 0, parentRevisions = [], nodeStatus = Head}

-- NewData instances with added context Enum k.  These were
-- extracted from -ddump-splices output.

instance (Enum k,
          N.Data ctx k,
          N.Data ctx Integer,
          N.Sat (ctx (Revision k)),
          N.Sat (ctx Integer)) =>
    N.Data ctx (Revision k) where
        gfoldl _ _f z x =
            case x of
              Revision argS7 argS8 -> _f (_f (z Revision) argS7) argS8
        gunfold _ _k z c =
            case N.constrIndex c of
              1 -> _k (_k (z Revision)) 
              _ -> error "gunfold: fallthrough"
        toConstr _ x = case x of Revision _ _ -> constrS1
        dataTypeOf _ _ = dataTypeS2

dataTypeS2 :: N.DataType
dataTypeS2 = N.mkDataType "Revision" [constrS1]
constrS1 :: N.Constr
constrS1 = N.mkConstr dataTypeS2 "Revision" ["ident", "number"] N.Prefix

instance (Enum k,
          N.Data ctx k,
          N.Data ctx (Revision k),
          N.Data ctx EpochMilli,
          N.Data ctx ([Integer]),
          N.Data ctx NodeStatus,
          N.Sat (ctx (RevisionInfo k)),
          N.Sat (ctx (Revision k)),
          N.Sat (ctx EpochMilli),
          N.Sat (ctx ([Integer])),
          N.Sat (ctx NodeStatus)) =>
    N.Data ctx (RevisionInfo k) where
        gfoldl _ _f z x =
            case x of
              { RevisionInfo argS18 argS19 argS20 argS21 ->
                    _f (_f (_f (_f (z RevisionInfo) argS18) argS19) argS20) argS21 }
        gunfold _ _k z c =
            case N.constrIndex c of
              1 -> _k (_k (_k (_k (z RevisionInfo))))
              _ -> error "gunfold: fallthrough"
        toConstr _ x =
            case x of { RevisionInfo _ _ _ _ -> constrS12 }
        dataTypeOf _ _ = dataTypeS13

dataTypeS13 :: N.DataType
dataTypeS13 = N.mkDataType "RevisionInfo" [constrS12]
constrS12 :: N.Constr
constrS12 = N.mkConstr dataTypeS13 "RevisionInfo" ["revision", "created", "parentRevisions", "nodeStatus"] N.Prefix
