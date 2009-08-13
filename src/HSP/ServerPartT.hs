{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module HSP.ServerPartT where

import HSP
import Control.Applicative
import Control.Monad.Identity
import qualified HSX.XMLGenerator as HSX
import Happstack.Server

instance (Monad m) => HSX.XMLGen (ServerPartT m) where
    type HSX.XML (ServerPartT m) = XML
    newtype HSX.Child (ServerPartT m) = SChild { unSChild :: XML }
    newtype HSX.Attribute (ServerPartT m) = SAttr { unSAttr :: Attribute }
    genElement n attrs children = 
        do attribs <- map unSAttr <$> asAttr attrs
           childer <- flattenCDATA . map unSChild <$> asChild children
           HSX.XMLGenT $ return (Element
                              (toName n)
                              attribs
                              childer
                             )
    xmlToChild = SChild

flattenCDATA :: [XML] -> [XML]
flattenCDATA cxml = 
                case flP cxml [] of
                 [] -> []
                 [CDATA _ ""] -> []
                 xs -> xs                       
    where
        flP :: [XML] -> [XML] -> [XML]
        flP [] bs = reverse bs
        flP [x] bs = reverse (x:bs)
        flP (x:y:xs) bs = case (x,y) of
                           (CDATA e1 s1, CDATA e2 s2) | e1 == e2 -> flP (CDATA e1 (s1++s2) : xs) bs
                           _ -> flP (y:xs) (x:bs)


instance (Monad m) => HSX.EmbedAsAttr (ServerPartT m) Attribute where
    asAttr = return . (:[]) . SAttr 

instance (Monad m) => HSX.EmbedAsAttr (ServerPartT m) (Attr String Char) where
    asAttr (n := c)  = asAttr (n := [c])

instance (Monad m) => HSX.EmbedAsAttr (ServerPartT m) (Attr String String) where
    asAttr (n := str)  = asAttr $ MkAttr (toName n, pAttrVal str)

instance (Monad m) => HSX.EmbedAsAttr (ServerPartT m) (Attr String Bool) where
    asAttr (n := True)  = asAttr $ MkAttr (toName n, pAttrVal "true")
    asAttr (n := False) = asAttr $ MkAttr (toName n, pAttrVal "false")

instance (Monad m) => HSX.EmbedAsAttr (ServerPartT m) (Attr String Int) where
    asAttr (n := i)  = asAttr $ MkAttr (toName n, pAttrVal (show i))

instance (Monad m) => EmbedAsChild (ServerPartT m) Char where
    asChild = XMLGenT . return . (:[]) . SChild . pcdata . (:[])

instance (Monad m) => EmbedAsChild (ServerPartT m) String where
    asChild = XMLGenT . return . (:[]) . SChild . pcdata

instance (Monad m) => EmbedAsChild (ServerPartT m) XML where
    asChild = XMLGenT . return . (:[]) . SChild

instance Monad m => EmbedAsChild (ServerPartT m) () where
  asChild () = return []

instance (Monad m) => AppendChild (ServerPartT m) XML where
 appAll xml children = do
        chs <- children
        case xml of
         CDATA _ _       -> return xml
         Element n as cs -> return $ Element n as (cs ++ (map unSChild chs))

instance (Monad m) => SetAttr (ServerPartT m) XML where
 setAll xml hats = do
        attrs <- hats
        case xml of
         CDATA _ _       -> return xml
         Element n as cs -> return $ Element n (foldr (:) as (map unSAttr attrs)) cs

instance (Monad m) => XMLGenerator (ServerPartT m)
