{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -F -pgmFtrhsx -Wwarn #-}
module HSP.Formlets.Util
    ( label
    , HSP.Formlets.Util.div
    , pre
    , field
    , typename
    , someof
    , oneof
    , forms
    , choice
{-  , XMLTrans(xmlTrans)
    , XMLTransD(xmlTransD) -}
    ) where

import HSP
import qualified HSX.XMLGenerator as HSX
import Control.Applicative.Error (Failing(..))
import Data.Generics.SYB.WithClass.Basics
import Data.Generics.SYB.WithClass.Instances ()
import Prelude hiding (div)
import HSP.Formlets
import Text.Formlets

label text frm = (\ xml -> [<div><% text %><% xml %></div>]) `plug` frm

-- |Wrap some XML in a div with the given class name.
-- div :: String -> XML -> XML
div cls = (\ xml -> [<div class=cls><% xml %></div>])
field c f xml = [<field constr=c name=f><% xml %></field>]
typename n xml = [<typename name=n><% xml %></typename>]
someof xml = [<someof><% xml %></someof>]
oneof xml = [<oneof><% xml %></oneof>]
forms xml = [<forms><% xml %></forms>]
choice xml = [<choice><% xml %></choice>]

pre s = xml [<pre><% s %></pre>]

{-
-- |A class for transforming XML according to a generic value.
class XMLTrans a where
    xmlTrans :: forall xml. a -> xml -> xml
    xmlTrans _ xml = xml

data XMLTransD a =
    XMLTransD { xmlTransD :: Plus xml => a -> xml -> xml }

xmlTransProxy :: Proxy XMLTransD
xmlTransProxy = error "urk"

instance XMLTrans a => Sat (XMLTransD a) where
    dict = XMLTransD { xmlTransD = xmlTrans }

instance XMLTrans (Set a) where
    xmlTrans _ xml = xml
-}
