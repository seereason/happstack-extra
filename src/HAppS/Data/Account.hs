{-# LANGUAGE TemplateHaskell, UndecidableInstances, DeriveDataTypeable, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, TypeSynonymInstances, GeneralizedNewtypeDeriving #-}
module HAppS.Data.Account where

import Control.Monad.Reader
import Data.Maybe
import Data.Generics
import HAppS.Data
import HAppS.Data.IxSet
import HAppS.Data.IxSet.Extra
import HAppS.Data.User.Password
import HAppS.State

$(deriveAll [''Eq, ''Ord, ''Show, ''Read]
  [d|
      data Account a = Account { username :: Username 
                               , password :: Password 
                               , acctData :: a
                               }
      newtype Username = Username String
    |])
$(deriveSerialize ''Username)
instance Version Username
$(deriveSerialize ''Account)
instance (Version a) => Version (Account a)

$(inferIxSet "Accounts" ''Account 'noCalcs [''Username])

-- |authenicate a user
-- On failure, returns Nothing.
-- On success, the account data for the user is returned.
authenticate :: (Data a, Ord a) => 
                String -- ^ username
             -> String -- ^ plain-text password 
             -> Query (Accounts a) (Maybe (Account a))
authenticate username password =
    do accts <- ask
       case getOne (accts @= (Username username)) of
         Nothing -> return Nothing
         (Just acct@(Account _ pw acctData)) -> 
             if checkPassword pw password
                then return (Just acct)
                else return Nothing

-- |create a new account
-- returns False if account already exists
-- returns True on success
create :: (Data a, Ord a) => Account a -> Update (Accounts a) Bool
create acct =
    testAndInsert (isNull . (@= (gFind' acct :: Username))) acct
    where
      isNull = isNothing . getOne

$(mkMethods ''Accounts 
                [ 'authenticate
                , 'create
                ])

-- delete NOTE: if an account is deleted, and a new account is created
-- with the same usernanme, will the new user have access to stuff
-- from the old account?