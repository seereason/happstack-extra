module HSP.Formlets.CamelCase where

--import Data.List
--import Data.Char
import Text.Regex
--import Test.QuickCheck


-- Add spaces to symbols of the form HelloThereWorld.

camelToText :: String -> String
{-
camelToText [] = []
camelToText (c:[]) = c:[]
camelToText (c:d:rest) | isLower c && isUpper d = c:' ':d:(camelToText rest)
                       | otherwise = c:(camelToText (d:rest))
-}

camelToText s = 
    case matchRegexAll (mkRegex "^([A-Za-z][a-z]*|[0-9]+)") s of
      Just (_, match, rest, _) -> match ++ [' '] ++ camelToText rest
      Nothing -> []

{-
QuickCheck doesn't support random strings?
This test wouldn't have been valid anyway.  Spaces in the input would have been filtered.

test1 :: String -> Property
test1 s1 = s1 == filter (/= ' ') (camelToText s1)
-}