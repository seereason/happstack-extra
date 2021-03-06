-- |Make URI an instance of Read and Ord, and add functions to
-- manipulate the uriQuery.
module Network.URI.Extra
    ( module Network.URI
    , relURI
    , setURIPort
    , parseURIQuery
    , modifyURIQuery
    , setURIQuery
    , setURIQueryAttr
    , deleteURIQueryAttr
    , modify
    , del
    , put
    , copy
    ) where

import Network.URI -- (URIAuth(..), URI(..), parseURI, uriToString, escapeURIString, isUnreserved, unEscapeString)
import Data.List(intersperse, groupBy, inits, partition)
import Data.Maybe (listToMaybe)
import Data.Maybe(isJust, isNothing, catMaybes)
import Control.Arrow(second)

-- |Create a relative URI with the given query.
relURI :: FilePath -> [(String, String)] -> URI
relURI path pairs = URI {uriScheme = "",
                   uriAuthority = Nothing,
                   uriPath = path,
                   uriQuery = formatURIQuery pairs,
                   uriFragment = ""}

-- |Set the port number in the URI authority, creating it if necessary.
setURIPort port uri =
    uri {uriAuthority = Just auth'}
    where
      auth' = auth {uriPort = port}
      auth = maybe nullAuth id (uriAuthority uri)
      nullAuth = URIAuth {uriUserInfo = "", uriRegName = "", uriPort = ""}

-- |Return the pairs in a URI's query
parseURIQuery :: URI -> [(String, String)]
parseURIQuery uri =
    case uriQuery uri of
      "" -> []
      '?' : attrs ->
          map (second (unEscapeString . tail) . break (== '='))
                  (filter (/= "&") (groupBy (\ a b -> a /= '&' && b /= '&') attrs))
      x -> error $ "Invalid URI query: " ++ x

-- |Modify a URI's query by applying a function to the pairs
modifyURIQuery :: ([(String, String)] -> [(String, String)]) -> URI -> URI
modifyURIQuery f uri = uri {uriQuery = formatURIQuery (f (parseURIQuery uri))}

setURIQuery :: [(String, String)] -> URI -> URI
setURIQuery pairs = modifyURIQuery (const pairs)

setURIQueryAttr :: String -> String -> URI -> URI
setURIQueryAttr name value uri =
    modifyURIQuery f uri
    where f pairs = (name, value) : filter ((/= name) . fst) pairs

deleteURIQueryAttr :: String -> URI -> URI
deleteURIQueryAttr name uri =
    modifyURIQuery f uri
    where f pairs = filter ((/= name) . fst) pairs

-- |Turn a list of attribute value pairs into a uriQuery.
formatURIQuery :: [(String, String)] -> String
formatURIQuery [] = ""
formatURIQuery attrs = '?' : concat (intersperse "&" (map (\ (a, b) -> a ++ "=" ++ escapeURIForQueryValue b) attrs))

-- |Escape a value so it can safely appear on the RHS of an element of
-- the URI query.  The isUnreserved predicate is the set of characters
-- that can appear in a URI which don't have any special meaning.
-- Everything else gets escaped.
escapeURIForQueryValue = escapeURIString isUnreserved

-- Make URI an instance of Read.  This will throw an error if no
-- prefix up to ten characters long of the argument string looks like
-- a URI.  If such a prefix is found, it will continue trying longer
-- and longer prefixes until the result no longer looks like a URI.
instance Read URI where
    readsPrec _ s =
        let allURIs = map parseURI (inits s) in
        -- If nothing in the first ten characters looks like a URI, give up
        case catMaybes (take 10 allURIs) of
          [] -> fail "read URI: no parse"
          -- Return the last element that still looks like a URI
          _ ->
              [(longestURI, drop (length badURIs + length goodURIs - 1) s)]
              where
                longestURI = case reverse (catMaybes goodURIs) of
                               [] -> error $ "Invalid URI: " ++ s
                               (a : _) -> a
                goodURIs = takeWhile isJust moreURIs
                (badURIs, moreURIs) = span isNothing allURIs

-- |Modify an individual URI query attributes.
modify :: String -> (Maybe String -> Maybe String) -> URI -> URI
modify a vf uri =
    let (vs, other) = partition ((== a) . fst) (parseURIQuery uri) in
    setURIQuery (case vf (listToMaybe (map snd vs)) of
                   Just v' -> (a, v') : other
                   Nothing -> other) uri

-- |Replace a query attribute with Nothing.
del :: String -> URI -> URI
del a uri = modify a (const Nothing) uri

-- |Replace a query attribute with something.
put :: String -> String -> URI -> URI 
put a v uri = modify a (const (Just v)) uri

-- |Copy an attribute from one query to another
copy :: String -> URI -> URI -> URI
copy a src dst = modify a (const (lookup a (parseURIQuery src))) dst

-- Apply f to all of the URI's pairs
{-
modifyAll :: (String -> Maybe String -> Maybe String) -> URI -> URI
modifyAll f uri =
    foldr (\ (a, _) uri -> modify a (f a) uri) uri pairs
    where
      pairs = parseURIQuery uri
-}
