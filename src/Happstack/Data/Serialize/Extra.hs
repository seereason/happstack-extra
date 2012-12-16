{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverlappingInstances, TemplateHaskell, UndecidableInstances #-}
{-# OPTIONS -fno-warn-orphans #-}
module Happstack.Data.Serialize.Extra where

import Data.SafeCopy (deriveSafeCopy, base)
import qualified Data.ByteString.UTF8 as B
import GHC.Exts (IsString(fromString))

newtype UTF8 = UTF8 {unUTF8 :: B.ByteString} deriving (Read, Show, Ord, Eq)

instance IsString UTF8 where
    fromString s = UTF8 $ B.fromString s

-- Disable these while we migrate any clients that used the old Serialize code
-- $(deriveSafeCopy 1 'base ''UTF8)
