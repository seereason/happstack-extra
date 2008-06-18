{-# LANGUAGE TemplateHaskell, UndecidableInstances, DeriveDataTypeable, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, TypeSynonymInstances, GeneralizedNewtypeDeriving #-}
module HAppS.Data.Session 
    ( -- * Basic Types
      SessionId(..)
    , Session(..)
    , Sessions
      -- * State functions
    , GetSession(..)
    , UpdateSession(..)
    , newSession
    )
    where

import Control.Monad.State hiding (State)
import Control.Monad.Reader
import Data.Generics
import Data.Maybe
import HAppS.Data
import HAppS.Data.IxSet
import HAppS.Data.IxSet.Extra
import HAppS.State
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

-- |attempt to start a new Session
tryNewSession :: (Data a, Ord a) => (Session a) -> Update (Sessions a) Bool
tryNewSession session =
    testAndInsert (isJust . getOne . (@= (gFind' session :: SessionId))) session

-- * methods

$(mkMethods ''Sessions 
  [ 'getSession
  , 'updateSession
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
