{-# OPTIONS_GHC -fglasgow-exts -F -pgmFtrhsx #-}
module HSP.Google.Analytics 
    ( UACCT(..)
    , analytics
    , addAnalytics
    ) where

import Data.Generics
import HSP
-- import HAppS.Template.HSP

newtype UACCT = UACCT String -- ^ The UACCT provided to you by Google
    deriving (Read, Show, Eq, Ord, Typeable, Data)

-- |create the google analytics script tags
-- NOTE: you must put the <% analytics yourUACCT %> immedialy for the </body> tag
-- See also: addAnalytics
analytics :: (XMLGenerator m) => UACCT -> GenXMLList m
-- analytics :: (Monad m) => UACCT -> HSPT m [XML]
analytics (UACCT uacct) =
    do a <- <script type="text/javascript">
              var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");
              document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));
            </script>
       b <- <script type="text/javascript">
              var pageTracker = _gat._getTracker("<% uacct %>");
              pageTracker._initData();
              pageTracker._trackPageview();
            </script>
       return [a,b]

-- |automatically add the google analytics scipt tags immediately before the </body> element
-- NOTE: this function is not idepotent
addAnalytics :: ( AppendChild m XML
                , EmbedAsChild m XML
                , EmbedAsAttr m Attribute
                , XMLGenerator m) 
             => UACCT 
             -> XMLGenT m XML 
             -> GenXML m
-- addAnalytics :: (Monad m) => UACCT -> HSPT m XML -> HSPT m XML
addAnalytics uacct pg =
    do page <- pg
       a <- analytics uacct
       case page of
         <html hattrs><[ head, body ]></html> ->
             <html hattrs>
                <% head %>
                <% body <: a %>
             </html>
         o -> error ("Failed to add analytics." ++ show o)

-- import HAppS.Template.HSP

{- Example Analytics Code from Google:
<script type="text/javascript">
var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");
document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));
</script>
<script type="text/javascript">
var pageTracker = _gat._getTracker("UA-4353757-1");
pageTracker._initData();
pageTracker._trackPageview();
</script>
-}

-- * Test
{-
testXML' :: Web XML -> IO XML
testXML' xml = evalHSP (runWebXML undefined xml)

testXML :: Web XML -> IO ()
testXML xml = evalHSP (runWebXML undefined xml) >>= putStrLn . renderAsHTML


-- dummy :: (Monad m) => HSPT m HSP.XML
-- dummy :: Web HSP.XML
dummy :: (EmbedAsChild m [Char]) => GenXML m
dummy =
    <html>
      <head>
        <title>the title</title>
      </head>
     <body>
       <p>the body</p>
     </body>
    </html>
-}

{-
-- * OLD

pageTemplate :: UACCT -> Web XML -> Web XML -> Web XML
pageTemplate uacct header body =
    do a <- analytics uacct
       hdr <- header
       (Element (Nothing, "body") attrs children) <- body
       h <- <html>
             <% hdr %>
             <% (Element (Nothing, "body") attrs (children ++ a)) %> 
            </html>
       return h

pageTest =
    pageTemplate (UACCT "hi") 
                 <head>
                   <title> I like Cheese!</title>
                 </head>
                 <body>
                   <p>bork brok brok</p>
                 </body>
-}
{-
addAnalytics :: UACCT -> Web XML -> Web XML
addAnalytics uacct page =
    case page of
      (Element (Nothing, "html") attrs children) ->
          return $ (Element (Nothing, "html") attrs 
          case find (\(Element (Nothing, name) _ _) -> name == "body") children of
            (Just (\ (Element (Nothing, name) attrs bodyChildren))) ->
                do a <- analytics uacct
                   return $ (Element (Nothing, name) attrs bodyChildren)
-}
