{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Happstack.Server.Extra 
    ( debug404
    , prettyRequest
    , prettyList
    , prettyDlist
    , withURI
    , withURISP
    , lookPairsPacked
    ) where

import Control.Applicative
import Control.Arrow ((***))
import Control.Monad(msum)
import Control.Monad.Reader (MonadPlus(..), ap, ReaderT(..), asks)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Lazy.UTF8 as U
import Data.Char (chr)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Happstack.Server as Happstack (RqData, Request(..), Response(..), ServerPartT(..), WebT(..), FilterMonad(..), ServerMonad(..), WebMonad(..), getHeader, multi, noopValidator
                   , notFound, setValidator, toResponse, withRequest, rqURL, runServerPartT, askRq) 
import HSP
import Happstack.Server.HTTP.Types (Input(inputValue))
import Network.URI (URI(URI), URIAuth(..), parseRelativeReference)
import Text.Html
import Text.Regex (mkRegexWithOpts, matchRegexAll)

-- |a 404 page which shows the failed Request as Html
debug404 :: (FilterMonad Response m, ServerMonad m, Monad m) => m Response
debug404 =
    askRq >>= \rq -> notFound (setValidator noopValidator (toResponse (prettyRequest rq)))

-- |pretty print the Request as Html
prettyRequest :: Happstack.Request -> Html
prettyRequest (Happstack.Request method paths uri query inputs cookies version headers body' peer)
          = thehtml ((thetitle (toHtml "404"))  +++
                     (body ((h1 (toHtml "Requested object not found.")) +++
                            (dlist
                             (((define (toHtml "method")  +++ (ddef (toHtml (show method)))))  +++
                              ((define (toHtml "paths")   +++ (ddef (prettyList (map toHtml paths))))) +++
                              ((define (toHtml "uri")     +++ (ddef (toHtml uri))))     +++
                              ((define (toHtml "query")   +++ (ddef (toHtml query))))          +++
                              ((define (toHtml "inputs")  +++ (ddef (prettyDlist (map (toHtml *** (toHtml . show)) inputs)))))  +++
                              ((define (toHtml "cookies") +++ (ddef (prettyDlist (map (toHtml *** (toHtml . show)) cookies)))))  +++
                              ((define (toHtml "version") +++ (ddef (toHtml (show version))))) +++
                              ((define (toHtml "headers") +++ (ddef (prettyDlist (map ((toHtml . show) *** (toHtml . show)) (Map.toList headers)))))) +++
                              ((define (toHtml "peer")    +++ (ddef (toHtml (show peer)))))    +++
                              ((define (toHtml "body")    +++ (ddef (toHtml (show body')))))
                             )
                            )
                           )
                     )
                    )

prettyList :: [Html] -> Html
prettyList = ulist . foldr (+++) noHtml . map li 

prettyDlist :: [(Html, Html)] -> Html
prettyDlist = dlist . foldr (+++) noHtml . map (\(k,v) -> define k +++ ddef v)

-- |Retrieve and parse the request URL and pass it to f.
withURI :: (URI -> WebT m a) -> ServerPartT m a
withURI f =
    withRequest (f . fromJust . parseRelativeReference . rqURL)
    -- The definition of rqURL in Happstack doesn't do what I would
    -- expect.  In fact, I don't really understand what it does.
    where rqURL rq = rqUri rq ++ rqQuery rq

-- |Retrieve and parse the request URL and pass it to f.
withURISP :: (ServerMonad m, Monad m) => (URI -> m a) -> m a
withURISP f = askRq >>= \request -> 
        let mHost = B.unpack <$> getHeader "host" request
            mAuthority =
                case mHost of
                  Nothing -> Nothing
                  (Just str) ->
                      case span (/= ':') str of
                        (host, port) -> Just (URIAuth "" host port)
            uri = (URI "http:" mAuthority (rqUri request) (rqQuery request) "" {- (rqFrag request) -})
        in f uri

-- |A version of Happstack lookPairs that doesn't unpack its values.
lookPairsPacked :: RqData [(String,L.ByteString)]
lookPairsPacked = asks fst >>= return . map (\ (n,vbs) -> (n, inputValue vbs))

-- * Simple Applicative and Alternative instances for RqData via ReaderT
instance (Monad m) => Applicative (ReaderT r m) where
    pure = return
    (<*>) = ap

instance (MonadPlus m) => Alternative (ReaderT r m) where
    empty = unwrapMonad empty
    f <|> g = unwrapMonad $ (WrapMonad f) <|> (WrapMonad g)

instance (ServerMonad m) => ServerMonad (XMLGenT m) where
    askRq   = XMLGenT askRq
    localRq f (XMLGenT m) = XMLGenT (localRq f m)

instance (FilterMonad a m) => FilterMonad a (XMLGenT m) where
    setFilter = XMLGenT . setFilter
    composeFilter f = XMLGenT (composeFilter f)
    getFilter (XMLGenT m) = XMLGenT (getFilter m)

instance (WebMonad a m) => WebMonad a (XMLGenT m) where
    finishWith r = XMLGenT $ finishWith r
