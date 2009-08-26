{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses #-}
module Happstack.Server.RqData where

import Control.Applicative 			(Applicative((<*>), pure))
import Control.Monad 				(Monad, MonadPlus(mzero))
import Control.Monad.Reader 			(ReaderT(ReaderT, runReaderT), MonadReader(ask, local), asks, mapReaderT)
import Control.Monad.Error 			(Error(noMsg, strMsg))
import qualified Data.ByteString.Lazy.Char8     as L
import qualified Data.ByteString.Lazy.UTF8      as LU
import Data.Char 				(toLower)
import Data.Monoid 				(Monoid(mempty, mappend, mconcat))
import Happstack.Server.Cookie 			(Cookie (cookieValue))
import Happstack.Server 			(Input(inputValue), Request(rqInputs, rqCookies), ServerMonad, askRq)
import Happstack.Util.Common                    (readM)

newtype ReaderError r e a = ReaderError { unReaderError :: ReaderT r (Either e) a }
    deriving (Functor, Monad)

instance (Error e) => MonadReader r (ReaderError r e) where
    ask = ReaderError ask
    local f m = ReaderError $ local f (unReaderError m)

instance (Monoid e, Error e) => Applicative (ReaderError r e) where
    pure = return
    (ReaderError (ReaderT f)) <*> (ReaderError (ReaderT a)) 
        = ReaderError $ ReaderT $ \env -> (f env) `apEither` (a env)

apEither :: (Monoid e) => Either e (a -> b) -> Either e a -> Either e b
apEither (Left errs1) (Left errs2) = Left (errs1 `mappend` errs2)
apEither (Left errs)  _            = Left errs
apEither _            (Left errs)  = Left errs
apEither (Right f)    (Right a)    = Right (f a)

newtype Errors a = Errors { unErrors :: [a] }

instance Monoid (Errors a) where
    mempty = Errors []
    (Errors x) `mappend` (Errors y) = Errors (x ++ y)
    mconcat errs = Errors $ concatMap unErrors errs

instance Error (Errors String) where
    noMsg = Errors []
    strMsg str = Errors [str]

type RqData = ReaderError ([(String, Input)], [(String, Cookie)]) (Errors String)

mapReaderErrorT :: (Either e a -> Either e' b) -> (ReaderError r e a) -> (ReaderError r e' b)
mapReaderErrorT f m = ReaderError $ mapReaderT f (unReaderError m)

readerError :: (Monoid e, Error e) => e -> ReaderError r e b
readerError e = mapReaderErrorT ((Left e) `apEither`) (return ())

runReaderError :: ReaderError r e a -> r -> Either e a
runReaderError = runReaderT . unReaderError

lookInput :: String -> RqData Input
lookInput name
    = do inputs <- asks fst
         case lookup name inputs of
           Just i  -> return $ i
           Nothing -> readerError (strMsg name)

-- | Gets the named input parameter as a lazy byte string
lookBS :: String -> RqData L.ByteString
lookBS = fmap inputValue . lookInput

look :: String -> RqData String
look = fmap LU.toString . lookBS

-- | Gets the named cookie
-- the cookie name is case insensitive
lookCookie :: String -> RqData Cookie
lookCookie name
    = do cookies <- asks snd
         case lookup (map toLower name) cookies of -- keys are lowercased
           Nothing -> fail "cookie not found"
           Just c  -> return c

-- | gets the named cookie as a string
lookCookieValue :: String -> RqData String
lookCookieValue = fmap cookieValue . lookCookie

-- | gets the named cookie as the requested Read type
readCookieValue :: Read a => String -> RqData a
readCookieValue name = readM =<< fmap cookieValue (lookCookie name)

-- | like look, but Reads for you.
lookRead :: Read a => String -> RqData a
lookRead name = readM =<< look name

-- | gets all the input parameters, and converts them to a string
lookPairs :: RqData [(String,String)]
lookPairs = asks fst >>= return . map (\(n,vbs)->(n,LU.toString $ inputValue vbs))


getDataFn :: (ServerMonad m) => RqData a -> m (Either [String] a)
getDataFn rqData = do
    rq <- askRq
    return $ either (Left . unErrors) Right $ runReaderError rqData (rqInputs rq, rqCookies rq)

withDataFn :: (MonadPlus m, ServerMonad m) => RqData a -> (a -> m r) -> m r
withDataFn fn handle = getDataFn fn >>= either (const mzero) handle
