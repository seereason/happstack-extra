{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module Happstack.Server.SimpleHTTP.Extra where

import Control.Applicative
import Control.Monad.Identity
--import Data.ByteString.Lazy.UTF8 as L
import qualified Data.ByteString.Char8 as P
import Happstack.Server hiding (Web)
import Happstack.Server.HSP.HTML ()
import HSP (XMLGenT, XML {-, XMLMetaData(..)-})
import HSP.HTML
import HSP.Identity
import qualified HSX.XMLGenerator as HSX

instance ToMessage (XMLGenT Identity XML) where
    toContentType _ = P.pack "text/html"
    toMessage xml   = toMessage (html4Strict, evalIdentity xml) -- L.fromString (renderAsHTML xml)