{-# LANGUAGE TemplateHaskell, UndecidableInstances, DeriveDataTypeable, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, TypeSynonymInstances, GeneralizedNewtypeDeriving #-}
-- |Simple account support. See http://src.seereason.com/examples/happs-logon-example/
module HAppS.Server.Account 
    ( Account(..)
    , Accounts(..)
    , UserId(..)
    , Username(..)
    , Authenticate(..)
    , Create(..)
    , defaultAccounts
    )
    where

import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import Data.Generics
import HAppS.Data
import HAppS.Data.IxSet
import HAppS.Data.IxSet.Extra
import HAppS.Data.User.Password
import HAppS.State
import Text.RJson

$(deriveAll [''Enum, ''Eq, ''Integral, ''Num, ''Ord, ''Read, ''Real, ''Show, ''Default]
  [d|
      newtype UserId = UserId Integer
    |])
$(deriveSerialize ''UserId)
instance Version UserId


instance ToJson UserId where
    toJson (UserId uId) = toJson uId

instance FromJson UserId where
    fromJson _  = fmap UserId . fromJson undefined


$(deriveAll [''Show, ''Read, ''Default, ''Eq, ''Ord]
  [d| 
      data Account a = Account { username :: Username 
                               , userId   :: UserId
                               , password :: Password
                               , acctData :: a
                               }
      newtype Username = Username { unUsername :: String }
   |])
$(deriveSerialize ''Username)
instance Version Username
$(deriveSerialize ''Account)
instance (Version a) => Version (Account a)

$(inferIxSet "AccountIxSet" ''Account 'noCalcs [''Username, ''UserId])

data (Data a, Ord a) => Accounts a
    = Accounts { nextId :: UserId
               , accountIxSet :: AccountIxSet a
               }
      deriving (Typeable, Data, Read, Show)

$(deriveSerialize ''Accounts)
instance (Version a) => Version (Accounts a)

defaultAccounts :: (Data a, Ord a) => Accounts a
defaultAccounts = Accounts { nextId = 0
                           , accountIxSet = empty
                           }

-- |authenicate a user
-- On failure, returns Nothing.
-- On success, the account data for the user is returned.
authenticate :: (Data a, Ord a) =>
                String -- ^ username
             -> String -- ^ plain-text password 
             -> Query (Accounts a) (Maybe (Account a))
authenticate username password =
    do accts <- liftM accountIxSet ask
       case getOne (accts @= (Username username)) of
         Nothing -> return Nothing
         (Just acct@(Account _ _ pw acctData)) -> 
             if checkPassword pw password
                then return (Just acct)
                else return Nothing

-- |create a new account
-- returns False if account already exists
-- returns True on success
create :: (Data a, Ord a) => Username -> Password -> a -> Update (Accounts a) (Either String UserId)
create username password acctData =
    do (Accounts nextId accountIxSet) <- get
       case (getOne (accountIxSet @= username)) of
         Just _ -> return (Left $ "username " ++ gFind' username ++ " already in use.")
         Nothing ->
             do put (Accounts (succ nextId) (insert (Account username nextId password acctData) accountIxSet))
                return (Right nextId)

$(mkMethods ''Accounts
                [ 'authenticate
                , 'create
                ])

-- delete NOTE: if an account is deleted, and a new account is created
-- with the same usernanme, will the new user have access to stuff
-- from the old account?
