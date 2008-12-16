{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module HAppS.Server.SimpleHTTP.Extra where

import Control.Applicative
import Control.Monad.Identity
--import Data.ByteString.Lazy.UTF8 as L
import qualified Data.ByteString.Char8 as P
import HAppS.Server hiding (Web)
import HAppS.Server.HSP.HTML ()
import HSP (XMLGenT, XML {-, XMLMetaData(..)-})
import HSP.HTML
import HSP.Identity
import qualified HSX.XMLGenerator as HSX

-- * Move to SimpleHTTP

instance Functor Result where
    fmap _ NoHandle = NoHandle
    fmap f (Ok g a) = Ok g (f a)
    fmap _ (Escape response) = Escape response

instance (Functor m) => Functor (WebT m) where
    fmap f (WebT m) = WebT (fmap (fmap f) m)

instance (Functor m, Monad m) => Applicative (WebT m) where
    pure = return
    (<*>) = ap


instance ToMessage (XMLGenT Identity XML) where
    toContentType _ = P.pack "text/html"
    toMessage xml   = toMessage (html4Strict, evalIdentity xml) -- L.fromString (renderAsHTML xml)
