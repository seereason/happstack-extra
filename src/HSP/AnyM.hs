{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, TypeFamilies, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans -F -pgmF trhsx #-}
module HSP.AnyM where

import HSP
import Control.Monad.Identity
import qualified HSX.XMLGenerator as HSX

newtype (Functor m, Monad m) => AnyM m a = AnyM { runAnyM :: m a }
    deriving (Functor, Monad)

instance (Monad m, Functor m) => HSX.XMLGenerator (AnyM m)

instance (Functor m, Monad m) => HSX.XMLGen (AnyM m) where
    type HSX.XML (AnyM m) = XML
    newtype HSX.Child (AnyM m) = IChild { unIChild :: XML }
    newtype HSX.Attribute (AnyM m) = IAttr { unIAttr :: Attribute }
    genElement n attrs children = HSX.XMLGenT $ 
                                  do attrs'    <- HSX.unXMLGenT (fmap (map unIAttr . concat) (sequence attrs))
                                     children' <- HSX.unXMLGenT (fmap (map unIChild . concat) (sequence children))
                                     return (Element (toName n) attrs' children')
    xmlToChild = IChild


instance (Monad m, Functor m) => HSX.EmbedAsAttr (AnyM m) Attribute where
    asAttr = return . (:[]) . IAttr 

instance (Monad m, Functor m) => HSX.EmbedAsAttr (AnyM m) (Attr String Char) where
    asAttr (n := c)  = asAttr (n := [c])

instance (Monad m, Functor m) => HSX.EmbedAsAttr (AnyM m) (Attr String String) where
    asAttr (n := str)  = asAttr $ MkAttr (toName n, pAttrVal str)

instance (Monad m, Functor m) => HSX.EmbedAsAttr (AnyM m) (Attr String Bool) where
    asAttr (n := True)  = asAttr $ MkAttr (toName n, pAttrVal "true")
    asAttr (n := False) = asAttr $ MkAttr (toName n, pAttrVal "false")

instance (Monad m, Functor m) => HSX.EmbedAsAttr (AnyM m) (Attr String Int) where
    asAttr (n := i)  = asAttr $ MkAttr (toName n, pAttrVal (show i))

instance (Monad m, Functor m) => EmbedAsChild (AnyM m) Char where
    asChild = XMLGenT . return . (:[]) . IChild . pcdata . (:[])

instance (Monad m, Functor m) => EmbedAsChild (AnyM m) String where
    asChild = XMLGenT . return . (:[]) . IChild . pcdata



instance (Monad m, Functor m) => EmbedAsChild (AnyM m) XML where
    asChild = XMLGenT . return . (:[]) . IChild

instance (Monad m, Functor m) => AppendChild (AnyM m) XML where
 appAll xml children = do
        chs <- children
        case xml of
         CDATA _ _       -> return xml
         Element n as cs -> return $ Element n as (cs ++ (map stripChild chs))

stripAttr :: (Monad m, Functor m) => HSX.Attribute (AnyM m) -> Attribute
stripAttr  (IAttr a) = a

stripChild :: (Monad m, Functor m) => HSX.Child (AnyM m) -> XML
stripChild (IChild c) = c

instance (Monad m, Functor m) => SetAttr (AnyM m) XML where
 setAll xml hats = do
        attrs <- hats
        case xml of
         CDATA _ _       -> return xml
         Element n as cs -> return $ Element n (foldr insert as (map stripAttr attrs)) cs

insert :: Attribute -> Attributes -> Attributes
insert = (:)

evalAnyM :: (Functor m, Monad m) => XMLGenT (AnyM m) XML -> m XML
evalAnyM = runAnyM . HSX.unXMLGenT

type AnyMXML m = XMLGenT (AnyM m) XML

page :: (Monad m, Functor m) => AnyMXML m
page = 
    <html>
     <head>
      <title>whee!</title>
     </head>
     <body>
      <p>whee</p>
     </body>
    </html>

test :: IO ()
test = evalAnyM page >>= putStrLn . renderAsHTML

test2 :: IO ()
test2 = putStrLn (renderAsHTML (runIdentity (evalAnyM page)))
