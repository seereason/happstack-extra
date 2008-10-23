{-# LANGUAGE TemplateHaskell, UndecidableInstances, DeriveDataTypeable, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, TypeSynonymInstances, GeneralizedNewtypeDeriving #-}
-- |Simple session support. See http://src.seereason.com/examples/happs-logon-example/
module HAppS.Server.Session 
    ( -- * Basic Types
      SessionId(..)
    , Session(..)
    , Sessions
      -- * State functions
    , GetSession(..)
    , UpdateSession(..)
    , DelSession(..)
    , newSession
    , withSessionId
    , withSessionData
    , withSessionDataSP
    , withMSessionId
    , withMSessionData
    , withMSessionDataSP
    )
    where

import Control.Applicative (optional)
import Control.Monad.Error (MonadIO)
import Control.Monad.State hiding (State)
import Control.Monad.Reader (liftIO, ask)
import Data.Generics (Data)
import Data.Maybe (fromJust, isNothing)
import HAppS.Data
import HAppS.Data.IxSet
import HAppS.Data.IxSet.Extra
import HAppS.State
import HAppS.Server (ServerPartT(..), WebT(..), anyRequest, withDataFn, webQuery, readCookieValue, noHandle, multi)
import HAppS.Server.Extra ()
import System.Random

$( deriveAll [''Ord, ''Eq, ''Read, ''Show, ''Default, ''Num]
   [d|
       newtype SessionId = SessionId Integer
    |])

$( deriveAll [''Ord, ''Eq, ''Read, ''Show, ''Default]
   [d|
       data Session a = Session SessionId a
    |])

$(inferIxSet "Sessions" ''Session 'noCalcs [''SessionId])

$(deriveSerialize ''Session)
instance (Version a) => Version (Session a)
$(deriveSerialize ''SessionId)
instance Version SessionId

-- |get the session data associated with the supplied SessionId
getSession :: (Data a, Ord a) => SessionId -> Query (Sessions a) (Maybe (Session a))
getSession sessionId = (return . getOne . (@= (sessionId :: SessionId))) =<< ask

-- |update the Session
updateSession :: (Data a, Ord a) => (Session a) -> Update (Sessions a) ()
updateSession session = modify (updateIx (gFind' session :: SessionId) session)

-- |delete the session associated with the sessionId
-- returns the deleted session (mostly because we need some place to disambiguate the session type)
delSession :: (Data a, Ord a) => SessionId -> Update (Sessions a) (Maybe (Session a))
delSession sessionId =
    do mSession <- (return . getOne . (@= (sessionId :: SessionId))) =<< get
       case mSession of
         Nothing -> return Nothing
         (Just session) -> 
             do modify (delete session)
                return (Just session)

-- |attempt to start a new Session
tryNewSession :: (Data a, Ord a) => (Session a) -> Update (Sessions a) Bool
tryNewSession session =
    testAndInsert (isNothing . getOne . (@= (gFind' session :: SessionId))) session

-- * methods

$(mkMethods ''Sessions 
  [ 'getSession
  , 'updateSession
  , 'delSession
  , 'tryNewSession
  ])

-- |start a new session with the supplied session data
-- returns: the SessionId
newSession :: (MonadIO m, Data a, Serialize a, Ord a) => a -> m SessionId
newSession sessData =
    do sessId <- liftIO $ fmap SessionId $ randomRIO (0,2^128)
       r <- update (TryNewSession (Session sessId sessData))
       if r
          then return sessId
          else newSession sessData


withSessionId :: (Monad m) => (SessionId -> [ServerPartT m r]) -> ServerPartT m r
withSessionId = withDataFn (readCookieValue "sessionId")

withSessionData :: (Ord a, Serialize a, Data a, MonadIO m) => SessionId -> (a -> WebT m r) -> WebT m r
withSessionData sID f =
    do mSessionData <- webQuery (GetSession sID)
       case mSessionData of
         Nothing -> noHandle
         (Just (Session _ sessionData)) ->
             f sessionData

withSessionDataSP' :: (Ord a, Serialize a, Data a, MonadIO m) => SessionId -> (a -> [ServerPartT m r]) -> ServerPartT m r
withSessionDataSP' sID f =
       do mSessionData <- liftIO (query (GetSession sID))
          case mSessionData of
            Nothing -> anyRequest noHandle
            (Just (Session _ sessionData)) ->
               multi (f sessionData)

withSessionDataSP :: (Ord a, Serialize a, Data a, MonadIO m) => (a -> [ServerPartT m r]) -> ServerPartT m r
withSessionDataSP f = withSessionId (\sID -> [withSessionDataSP' sID f])

withMSessionId :: (Monad m) => (Maybe SessionId -> [ServerPartT m r]) -> ServerPartT m r
withMSessionId = withDataFn (optional (readCookieValue "sessionId"))

withMSessionData :: (Ord a, Serialize a, Data a, MonadIO m) => SessionId -> (Maybe a -> WebT m r) -> WebT m r
withMSessionData sID f =
    do mSessionData <- webQuery (GetSession sID)
       case mSessionData of
         Nothing -> f Nothing
         (Just (Session _ sessionData)) ->
             f (Just sessionData)

withMSessionDataSP' :: (Ord a, Serialize a, Data a, MonadIO m) => Maybe SessionId -> (Maybe a -> [ServerPartT m r]) -> ServerPartT m r
withMSessionDataSP' Nothing f = multi (f Nothing)
withMSessionDataSP' (Just sID) f =
    do mSessionData <- liftIO . query . GetSession $ sID
       case mSessionData of
         Nothing -> multi (f Nothing)
         (Just (Session _ sessionData)) ->
             multi (f (Just sessionData))

withMSessionDataSP :: (Ord a, Serialize a, Data a, MonadIO m) => (Maybe a -> [ServerPartT m r]) -> ServerPartT m r
withMSessionDataSP f =
    withMSessionId (\sID -> [withMSessionDataSP' sID f])
    where
