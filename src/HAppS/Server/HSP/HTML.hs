{-# LANGUAGE FlexibleInstances #-}
module HAppS.Server.HSP.HTML where

import qualified Data.ByteString.Char8 as P
import qualified Data.ByteString.Lazy.UTF8 as L
import HAppS.Server(ToMessage(toMessage, toContentType))
import HSP.XML (XML(..), XMLMetaData(..))
import HSP.HTML (html4Strict, renderAsHTML)

instance ToMessage XML where
    toContentType _ = P.pack "text/html"
    toMessage xml   = toMessage (html4Strict, xml)

instance ToMessage (Maybe XMLMetaData, XML) where
    toContentType (Just md,_) = P.pack (contentType md)
    toContentType _ = P.pack "text/html"
    toMessage (Just (XMLMetaData (showDt, dt) _ pr), xml) = 
        L.fromString ((if showDt then (dt ++) else id) (pr xml))
    toMessage (Nothing, xml) =
        L.fromString (renderAsHTML xml)
