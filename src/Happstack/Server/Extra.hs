{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
{-# OPTIONS -fno-warn-orphans #-}
module Happstack.Server.Extra 
    ( debug404
    , prettyRequest
    , prettyList
    , prettyDlist
    , withURI
    , withURISP
    , lookPairsPacked
    , lookPairsUnicode
    ) where

import Control.Applicative
import Control.Arrow ((***))
--import Control.Monad(msum)
import Control.Monad.Reader (MonadPlus(..), ap, ReaderT(..), asks)
import qualified Data.ByteString.Char8 as B
--import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Lazy.UTF8 as U
--import Data.Char (chr)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Happstack.Server as Happstack (RqData, Request(..), Response(..), ServerPartT(..), FilterMonad(..), ServerMonad(..), WebMonad(..), HasRqData(..), getHeader, lookPairs, noopValidator, notFound, setValidator, toResponse) 
import HSP
import Happstack.Server.Types (Input(inputValue))
import Network.URI (URI(URI), URIAuth(..), parseRelativeReference)
import Text.Html
import Text.Html.Entities (utf8ToUnicode)
--import Text.Regex (mkRegexWithOpts, matchRegexAll)

-- |a 404 page which shows the failed Request as Html
debug404 :: (FilterMonad Response m, ServerMonad m, Monad m) => m Response
debug404 =
    askRq >>= \rq -> notFound (setValidator noopValidator (toResponse (prettyRequest rq)))

-- |pretty print the Request as Html
prettyRequest :: Happstack.Request -> Html
prettyRequest (Happstack.Request method paths uri query inputsQuery inputsBody cookies version headers body' peer)
          = thehtml ((thetitle (toHtml "404"))  +++
                     (body ((h1 (toHtml "Requested object not found.")) +++
                            (dlist
                             (((define (toHtml "method")  +++ (ddef (toHtml (show method)))))  +++
                              ((define (toHtml "paths")   +++ (ddef (prettyList (map toHtml paths))))) +++
                              ((define (toHtml "uri")     +++ (ddef (toHtml uri))))     +++
                              ((define (toHtml "query")   +++ (ddef (toHtml query))))          +++
                              ((define (toHtml "inputs")  +++ (ddef (prettyDlist (map (toHtml *** (toHtml . show)) inputsQuery)))))  +++
                              ((define (toHtml "cookies") +++ (ddef (prettyDlist (map (toHtml *** (toHtml . show)) cookies)))))  +++
                              ((define (toHtml "version") +++ (ddef (toHtml (show version))))) +++
                              ((define (toHtml "headers") +++ (ddef (prettyDlist (map ((toHtml . show) *** (toHtml . show)) (Map.toList headers)))))) +++
                              ((define (toHtml "peer")    +++ (ddef (toHtml (show peer))))) --    +++
--                              ((define (toHtml "body")    +++ (ddef (toHtml (show body')))))
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
withURI :: (ServerMonad m) => (URI -> m a) -> m a
withURI f =
    do rq <- askRq
       (f . fromJust . parseRelativeReference . rqURL) $ rq
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
lookPairsPacked :: RqData [(String, Either FilePath U.ByteString)]
lookPairsPacked =
    do (query, body, _cookies) <- askRqEnv
       return $ map (\(n,vbs)->(n, inputValue vbs)) (query ++ body)

-- |Interpret the packed string returned from the server as UTF8
-- (which it is) and convert it to unicode.
lookPairsUnicode :: RqData [(String, Either FilePath String)]
lookPairsUnicode = lookPairs
{-# DEPRECATED lookPairsUnicode "just use lookPairs" #-}
