{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, TemplateHaskell #-}
{-# OPTIONS -fno-warn-orphans #-}
module Happstack.Data.Serialize.Extra where

import Happstack.Data (Default(..), deriveAll, deriveSerialize)
import Happstack.State (Version(..))
import qualified Data.ByteString.UTF8 as B
import GHC.Exts (IsString(fromString))

$(deriveAll [''Read, ''Show, ''Default, ''Ord, ''Eq]
  [d|
      newtype UTF8 = UTF8 {unUTF8 :: B.ByteString}
 |])

instance Version UTF8
$(deriveSerialize ''UTF8)

instance IsString UTF8 where
    fromString s = UTF8 $ B.fromString s
