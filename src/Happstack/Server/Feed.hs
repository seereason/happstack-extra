module Happstack.Server.Feed where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.UTF8 as L
import Happstack.Server(ToMessage(..))
import Text.RSS.Export (xmlRSS)
import Text.RSS.Syntax (RSS(..))
import Text.XML.Light (showTopElement)

instance ToMessage RSS where
    toContentType _ = C.pack "application/rss+xml;charset=utf-8"
    toMessage rss =
        L.fromString (showTopElement (xmlRSS rss))
