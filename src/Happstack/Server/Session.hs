{-# LANGUAGE TemplateHaskell, UndecidableInstances, DeriveDataTypeable, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, TypeSynonymInstances, GeneralizedNewtypeDeriving #-}
-- |Simple session support
module Happstack.Server.Session 
    ( -- * Basic Types
      SessionData
    , SessionId(..)
    , Session(..)
    , Sessions
      -- * State functions
    , GetSession(..)
    , UpdateSession(..)
    , DelSession(..)
    , NewSession(..)
    , withSessionId
    , withSessionData
    , withSessionDataSP
    , withMSessionId
    , withMSessionData
    , withMSessionDataSP
    )
    where

import Control.Applicative ((<$>),optional)
--import Control.Monad (MonadPlus)
--import Control.Monad.Error (MonadIO)
import Control.Monad.State hiding (State)
import Control.Monad.Reader (ask)
import Data.Generics (Data)
import Data.Maybe (isNothing)
import Happstack.Data (Default, deriveAll, gFind')
import Happstack.Data.IxSet (Indexable(..), (@=), delete, getOne, inferIxSet, noCalcs, updateIx)
import Happstack.Data.IxSet.Extra (testAndInsert)
import Happstack.State (Serialize, Version, Query, Update, deriveSerialize, getRandom, mkMethods, query)
import Happstack.Server (ServerMonad, withDataFn, readCookieValue)
import Happstack.Server.Extra ()

class (Ord s, Serialize s, Data s, Default s) => SessionData s

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

-- |start a new session with the supplied session data
-- returns: the SessionId
newSession :: (Data a, Serialize a, Ord a) => a -> Update (Sessions a) SessionId
newSession sessData =
    do sessId <- SessionId <$> getRandom
       let session = (Session sessId sessData)
       r <- testAndInsert (isNothing . getOne . (@= sessId)) session
       if r
          then return sessId
          else newSession sessData

-- * methods

$(mkMethods ''Sessions 
  [ 'getSession
  , 'updateSession
  , 'delSession
  , 'newSession
  ])

withSessionId :: (MonadPlus m, ServerMonad m) => (SessionId -> m a) -> m a
withSessionId = withDataFn (readCookieValue "sessionId")

withSessionData :: (Ord a, Serialize a, Data a, MonadIO m, MonadPlus m) => SessionId -> (a -> m r) -> m r
withSessionData sID f =
    do mSessionData <- query (GetSession sID)
       case mSessionData of
         Nothing -> mzero
         (Just (Session _ sessionData)) ->
             f sessionData

withSessionDataSP' :: (Ord a, Serialize a, Data a, MonadIO m, MonadPlus m) => SessionId -> (a -> m r) -> m r
withSessionDataSP' sID f =
       do mSessionData <- query (GetSession sID)
          case mSessionData of
            Nothing -> mzero
            (Just (Session _ sessionData)) ->
                f sessionData

withSessionDataSP :: (Ord a, Serialize a, Data a, MonadIO m, ServerMonad m, MonadPlus m) => (a -> m r) -> m r
withSessionDataSP f = withSessionId (\sID -> withSessionDataSP' sID f)

withMSessionId :: (ServerMonad m, MonadPlus m) => (Maybe SessionId -> m r) -> m r
withMSessionId f = withDataFn (optional (readCookieValue "sessionId")) $ \mSid -> f mSid

withMSessionData :: (Ord a, Serialize a, Data a, MonadIO m) => SessionId -> (Maybe a -> m r) -> m r
withMSessionData sID f =
    do mSessionData <- query (GetSession sID)
       case mSessionData of
         Nothing -> f Nothing
         (Just (Session _ sessionData)) ->
             f (Just sessionData)

withMSessionDataSP' :: (Ord a, Serialize a, Data a, MonadIO m) => Maybe SessionId -> (Maybe a -> m r) -> m r
withMSessionDataSP' Nothing f = f Nothing
withMSessionDataSP' (Just sID) f =
    do mSessionData <- query . GetSession $ sID
       case mSessionData of
         Nothing -> f Nothing
         (Just (Session _ sessionData)) ->
             f (Just sessionData)

withMSessionDataSP :: (Ord a, Serialize a, Data a, MonadIO m, ServerMonad m, MonadPlus m) => (Maybe a -> m r) -> m r
withMSessionDataSP f =
    withMSessionId (\sID -> withMSessionDataSP' sID f)
    where
