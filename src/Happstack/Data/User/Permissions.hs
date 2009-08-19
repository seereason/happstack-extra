{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             OverlappingInstances, TemplateHaskell, UndecidableInstances #-}
{-# OPTIONS -Wwarn -fno-warn-orphans #-}
-- |The user permissions module I threw together for the appraisal
-- report generator.  Improvements welcome.
module Happstack.Data.User.Permissions 
    ( Permissions(..)
    , readable
    , writable
    , owns
    , username
    , unUsername
    , Ownable(..)
    , setCopyPermissions
    , readScrub
    , writeScrub
    ) where

--  FlexibleInstances, MultiParamTypeClasses

import Control.Applicative ((<$>), (<*>))
import Control.Applicative.Error (Failing(Success))
import Data.Generics (Data, Typeable)
import Data.List (intercalate)
import Happstack.Data (deriveSerialize, deriveNewData, Default(..), Version)
import Happstack.Server.Account (Username(Username))
import Happstack.Server.Session (Session(..))
{-
import Text.Formlets (check, plug)
import Text.Formlets.Generics -- (FormletOf(formletOf), FormletType(V), formlet3)
import Text.Formlets.Generics.Inputs (text)
import Text.Formlets.Generics.Instances.List (appTail, remTail)
-}

{-
instance FormletOf Permissions where
    formletOf x@(Permissions a b c) = formlet3 x Permissions a b c
-}

-- We use the Username here rather than  the UserId so we don't
-- have to consult the database to convert this into something 
-- we can display in a form.
data Permissions
    = Permissions
      { owner :: Username,
        writers :: [Username],      -- Sets would be better here, but we need a better formletOfSet implementation first
        readers :: [Username]
      } deriving (Read, Show, Eq, Ord, Typeable, Data)

$(deriveNewData [''Permissions])
$(deriveSerialize ''Permissions)
instance Version Permissions

-- True if report has no permissions structure,
-- False if no user is logged in
-- Otherwise check user list against authorized readers
readable :: Maybe Permissions -> Maybe Username -> Bool
readable perms user = 
    maybe True (\ perms' -> maybe False (readable' perms') user) perms
    where readable' perms user = elem user (owner perms : writers perms ++ readers perms)

writable :: Maybe Permissions -> Maybe Username -> Bool
writable perms user = 
    maybe True (\ perms' -> maybe False (writable' perms') user) perms
    where writable' perms user = elem user (owner perms : writers perms)

owns :: Maybe Permissions -> Maybe Username -> Bool
owns perms user = 
    maybe True (\ perms' -> maybe False (owns' perms') user) perms
    where owns' perms user = user == owner perms

-- FIXME: Any name that isn't all whitespace is ok for now.  We should
-- really check against the actual list of existing users.
{-
goodUsername (Username s) = not . isJust . matchRegex re $ s
    where re = mkRegex "^[ \t]*$"
-}

-- Enforce some reasonable limitations on the format of a user name:
-- It can't be all whitespace, leading or trailing whitespace is removed,
-- It embedded whitespace is changed to a single space.
username :: String -> Maybe Username
username s = case words s of
               [] -> Nothing
               ws -> Just . Username $ intercalate " " ws

unUsername :: Username -> String
unUsername (Username x) = x

class Ownable a where
    getPermissions :: a -> Maybe Permissions
    putPermissions :: Maybe Permissions -> a -> a

-- When we make a copy of something, the copy's ownership is the current
-- user, not the owner of the original.
setCopyPermissions :: Ownable a => Maybe Username -> a -> Either String a
setCopyPermissions Nothing _ = Left "You must be logged in to do that"
setCopyPermissions (Just u) x =
    case getPermissions x of
      Nothing -> Right x
      Just perms -> Right (putPermissions (Just (perms {owner = u})) x)

-- |Example permissions scrubbing functions.  These return Nothing if
-- you lack permissions for the object.  You might instead want a function
-- that erased only the sensitive parts of the value.
readScrub :: Ownable a => Maybe Username -> a -> Maybe a
readScrub user x =
    case readable (getPermissions x) user of
      True -> Just x
      False -> Nothing

writeScrub :: Ownable a => Maybe Username -> a -> Maybe a
writeScrub user x =
    case writable (getPermissions x) user of
      True -> Just x
      False -> Nothing
