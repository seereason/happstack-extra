{-# LANGUAGE TypeSynonymInstances #-}
-- |used to fix broken cookie parsing is older versions of happstack. Not need in happstack >= 0.2.
module Happstack.Server.CookieFixer 
    ( cookieFixer
    ) where

import Happstack.Server.Cookie (Cookie(..))
import Happstack.Server(ServerMonad, ServerPartT(..),Request(..), getHeader, localRq)

import qualified Data.ByteString.Char8 as C
import Data.Char (chr, toLower)
import Data.List ((\\))
import Data.Maybe

import Control.Applicative
import Control.Monad (MonadPlus(..), ap)
-- Hide Parsec's definitions of some Applicative functions.
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))

-- Less complete but more robust way of parsing cookies.  Note: not RFC 2068 compliant!
parseCookies :: String -> [Cookie]
parseCookies str = either (const []) id $ parse cookiesParser "" str

parseCookiesM :: (Monad m) => String -> m [Cookie]
parseCookiesM str = either (fail "Invalid cookie syntax!") return $ parse cookiesParser str str

cookiesParser :: CharParser () [Cookie]
cookiesParser = av_pairs
    where -- Parsers based on RFC 2109
          av_pairs      = (:) <$> av_pair <*> many (char ';' *>  av_pair)
          av_pair       = cookie <$> attr <*> option "" (char '=' *> value)
          attr          = spaces *> token
          value         = word
          word          = incomp_token <|> quoted_string

          -- Parsers based on RFC 2068
          token         = many1 $ oneOf ((chars \\ ctl) \\ tspecials)
          quoted_string = char '"' *> many (oneOf qdtext) <* char '"'

          -- Custom parser, incompatible with RFC 2068, but very  forgiving ;)
          incomp_token  = many1 $ oneOf ((chars \\ ctl) \\ "\";")

          -- Primitives from RFC 2068
          tspecials     = "()<>@,;:\\\"/[]?={} \t"
          ctl           = map chr (127:[0..31])
          chars         = map chr [0..127]
          octet         = map chr [0..255]
          text          = octet \\ ctl
          qdtext        = text \\ "\""

cookie key value = Cookie "" "" "" (low key) value
{-
simpleHTTPCookieFixer :: ToMessage a => Conf -> [ServerPartT IO a] -> IO ()
simpleHTTPCookieFixer conf hs
    = listen conf (\req -> runValidator (fromMaybe return (validator conf)) =<< simpleHTTP' hs (cookieFixer req))

cookieFixer :: Request -> Request
cookieFixer request = [ (cookieName c, c) | cl <- fromMaybe [] (fmap getCookies (getHeader "Cookie" (rqHeaders request))), c <- cl ]
-}

cookieFixer :: (ServerMonad m) => m a -> m a
cookieFixer = localRq (\request -> request { rqCookies = (fixedCookies request) })
    where
      fixedCookies request = [ (cookieName c, c) | cl <- fromMaybe [] (fmap getCookies (getHeader "Cookie" (rqHeaders request))), c <- cl ]

-- | Get all cookies from the HTTP request. The cookies are ordered per RFC from
-- the most specific to the least specific. Multiple cookies with the same
-- name are allowed to exist.
getCookies :: Monad m => C.ByteString -> m [Cookie]
getCookies header | C.null header = return []
                  | otherwise     = parseCookiesM (C.unpack header)

-- | Get the most specific cookie with the given name. Fails if there is no such
-- cookie or if the browser did not escape cookies in a proper fashion.
-- Browser support for escaping cookies properly is very diverse.
getCookie :: Monad m => String -> C.ByteString -> m Cookie
getCookie s h = do cs <- getCookies h
                   case filter ((==) (low s) . cookieName) cs of
                     [r] -> return r
                     _   -> fail ("getCookie: " ++ show s)

low :: String -> String
low = map toLower