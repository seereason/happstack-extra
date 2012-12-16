{-# LANGUAGE TemplateHaskell,DeriveDataTypeable,FlexibleInstances,MultiParamTypeClasses,FlexibleContexts,UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Happstack.Data.User.Password
-- Copyright   :  (c) 2008 Jeremy Shaw <jeremy@n-heptane.com>
-- License     :  BSD3-style
-- 
-- Maintainer  :  Jeremy Shaw, jeremy@n-heptane.com
-- Stability   :  experimental
-- Portability :  requires all sorts of crazy GHC extensions
--
-- Data types and functions for handling salted passwords.
-----------------------------------------------------------------------------
module Happstack.Data.User.Password where

import Control.Monad
import Happstack.Crypto.SHA1
import Data.SafeCopy (deriveSafeCopy, base)
import System.Random

data    Password     = Password Salt PasswordHash deriving (Ord, Eq, Read, Show)
newtype PasswordHash = PasswordHash String deriving (Ord, Eq, Read, Show)
newtype Salt         = Salt String deriving (Ord, Eq, Read, Show)

-- |check if the submitted password matches the stored password
checkPassword :: Password -- ^ stored salt and password hash
              -> String  -- ^ password to test (unhashed)
              -> Bool -- ^ did it match
checkPassword (Password salt hash) password =
    doHash salt password == hash

-- |hash a password using the supplied salt
doHash :: Salt -> String -> PasswordHash
doHash (Salt salt) password =
    PasswordHash (sha1 (salt ++ password))

-- |generate some random salt
-- returns 4 'Char' of salt.
genSalt :: IO Salt
genSalt = liftM Salt $ replicateM 4 randomIO

-- |generate a new salted/hashed 'Password' from the given input string
newPassword :: String -> IO Password
newPassword password =
    do salt <- genSalt
       return (Password salt (doHash salt password))

-- |a 'Password' for which 'checkPassword' will always return 'False'
lockedPassword :: Password
lockedPassword = Password (Salt "") (PasswordHash "")

-- Disable these while we migrate any clients that used the old Serialize code
-- $(deriveSafeCopy 0 'base ''Password)
-- $(deriveSafeCopy 0 'base ''PasswordHash)
-- $(deriveSafeCopy 0 'base ''Salt)
