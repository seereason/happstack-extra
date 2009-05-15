{-# LANGUAGE FlexibleContexts, FlexibleInstances, PatternGuards, RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS -Wall -fwarn-missing-signatures -fno-warn-name-shadowing -fwarn-unused-imports #-}
module Happstack.Server.Formlets
    ( createForm
    , handleForm
    , withDatumSP
    ) where

import Control.Applicative.Error (Failing(..))
import Control.Arrow (second)
import Control.Monad (msum)
import Control.Monad.Trans (MonadIO, lift)
import Happstack.Server (methodSP, methodM, Method(GET, POST), ok, toResponse, withDataFn,
                         WebT, Response, ServerPartT(..), anyRequest, notFound, RqData)
import Happstack.Server.Extra (lookPairsUnicode)
import Happstack.Server.SimpleHTTP.Extra ()
import Happstack.State (QueryEvent, query)
import Data.Generics.SYB.WithClass.Instances ()
import Extra.URI (URI)
import Prelude hiding (div, maybe)
import Text.Formlets (Form, Env, runFormState)

-- |handleForm - a convenience function for using Formlets in Happstack
handleForm :: forall a. forall xml1 forall xml2. forall m. forall r. (Monad m) =>
              String -- ^ prefix
           -> URI -- ^ url for action attribute in form
           -> (xml2 -> r) -- ^ function to render the page that the form goes in
           -> Form xml1 m a -- ^ the form
           -> (a -> WebT m r) -- ^ function which handles POSTed results that successfully validate
           -> (Env -> URI -> [String] -> xml1 -> xml2)
           -> ServerPartT m r
handleForm prefix action page frm handleOk formXML = msum
    [ methodM GET >> ok (page (createForm [] prefix action [] frm formXML))
    , withDataFn lookPairsUnicode $ \env' ->
        methodSP POST $ anyRequest $
                 do let env = map (second Left) env'
                        (extractor, _, _) = runFormState env prefix frm                        
                    res <- lift extractor
                    case res of
                      Failure faults ->
                          ok (page (createForm env prefix action faults frm formXML))
                      Success s -> handleOk s
    ]

createForm :: forall a. forall xml1. forall xml2. forall m. (Monad m) => Env -> String -> URI -> [String] -> Form xml1 m a -> (Env -> URI -> [String] -> xml1 -> xml2) -> xml2
createForm env prefix action' faults frm formXML =
    -- TheForm env action' faults xml 
    formXML env action' faults xml
    where
      (_extractor, xml, _ct) = runFormState env prefix frm

-- |Try to extract a datum from the request data and pass it to some
-- server parts.  This is a generic Happstack function, not formlet specific,
-- so it probably belongs in Happstack-Extra.
withDatumSP :: (Show a, QueryEvent ev (Maybe t), MonadIO m) =>
               RqData a -> (a -> ev) -> (t -> [ServerPartT m Response]) -> ServerPartT m Response
withDatumSP look queryEv f =
    withDataFn look $ \ x ->
        do mResult <- query (queryEv x)
           case mResult of
             Nothing -> anyRequest (notFound (toResponse ("Invalid argument: " ++ show x)))
             Just result -> msum (f result)
