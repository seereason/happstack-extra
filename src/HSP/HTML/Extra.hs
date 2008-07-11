module HSP.HTML.Extra where

import HSP

showHTML :: HSP XML -> IO String
showHTML page = 
    do (xmd, xml) <- evalHSP page Nothing
       case xmd of
         Nothing -> 
             return $ renderAsHTML xml
         (Just (XMLMetaData (showDt, dt) _ pr)) ->
             return $ ((if showDt then (dt ++) else id) (pr xml))