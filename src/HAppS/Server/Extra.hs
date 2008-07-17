module HAppS.Server.Extra where

import HAppS.Server
import Text.Html

-- |a 404 page which shows the failed Request as Html
debug404 :: (Monad m) => ServerPartT m Response
debug404 =
    withRequest $ \rq -> notFound (toResponse (prettyRequest rq))

-- |pretty print the Request as Html
prettyRequest :: Request -> Html
prettyRequest (Request method paths uri query inputs cookies version headers body' peer)
          = thehtml ((thetitle (toHtml "404"))  +++
                     (body ((h1 (toHtml "Requested object not found.")) +++
                            (table
                             ((tr (td (toHtml "method") +++ (td (toHtml (show method))))) +++
                              (tr (td (toHtml "paths") +++ (td (toHtml (show paths))))) +++
                              (tr (td (toHtml "uri") +++ (td (toHtml (show uri))))) +++
                              (tr (td (toHtml "query") +++ (td (toHtml query)))) +++
                              (tr (td (toHtml "inputs") +++ (td (toHtml (show inputs))))) +++
                              (tr (td (toHtml "cookies") +++ (td (toHtml (show cookies))))) +++
                              (tr (td (toHtml "version") +++ (td (toHtml (show version))))) +++
                              (tr (td (toHtml "headers") +++ (td (toHtml (show headers))))) +++
                              (tr (td (toHtml "peer") +++ (td (toHtml (show peer))))) +++
                              (tr (td (toHtml "body") +++ (td (toHtml (show body')))))
                             )
                            )
                           )
                     )
                    )

