{-# LANGUAGE TemplateHaskell,DeriveDataTypeable,FlexibleInstances,MultiParamTypeClasses,FlexibleContexts,UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  HAppS.Data.User.Password
-- Copyright   :  (c) 2008 Jeremy Shaw <jeremy@n-heptane.com>
-- License     :  BSD3-style
-- 
-- Maintainer  :  Jeremy Shaw, jeremy@n-heptane.com
-- Stability   :  experimental
-- Portability :  requires all sorts of crazy GHC extensions
--
-- Data types and functions for handling salted passwords.
-----------------------------------------------------------------------------
module HAppS.Data.User.Password where

import Control.Monad
import HAppS.Crypto.SHA1
import HAppS.Data
import System.Random

$( deriveAll [''Ord,''Eq,''Read,''Show,''Default] 
   [d|
       data    Password     = Password Salt PasswordHash
       newtype PasswordHash = PasswordHash String
       newtype Salt         = Salt String
    |] )

$(deriveSerialize ''Password)
instance Version Password
$(deriveSerialize ''PasswordHash)
instance Version PasswordHash
$(deriveSerialize ''Salt)
instance Version Salt

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