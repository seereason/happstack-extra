{-# LANGUAGE TemplateHaskell, UndecidableInstances, DeriveDataTypeable, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, TypeSynonymInstances, GeneralizedNewtypeDeriving #-}
-- |Simple account support. See http://src.seereason.com/examples/happs-logon-example/
module Happstack.Server.Account 
    ( AccountData
    , Account(..)
    , Accounts(..)
    , UserId(..)
    , Username(..)
    , Authenticate(..)
    , Create(..)
    , defaultAccounts
    , AcctsFromIds(..)
    , AcctsFromUsers(..)
    , AcctFromId(..)
    , ChangePassword(..)
    )
    where

import Control.Applicative ((<$>))
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import Data.Generics
import Happstack.Data
import Happstack.Data.IxSet
--import Happstack.Data.IxSet.Extra
import Happstack.Data.User.Password
import Happstack.State
import Text.RJson

class (Ord a, Serialize a, Data a, Default a) => AccountData a

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
defaultAccounts = Accounts { nextId = 1
                           , accountIxSet = empty
                           }

-- |authenicate a user
-- On failure, returns Nothing.
-- On success, the account data for the user is returned.
authenticate :: (Data a, Ord a) =>
                String ->
                String ->
                Query (Accounts a) (Maybe (Account a))
authenticate username password =
    do accts <- liftM accountIxSet ask
       case getOne (accts @= (Username username)) of
         Nothing -> return Nothing
         (Just acct@(Account _ _ pw _acctData)) -> 
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

acctsFromIds :: (Data a, Ord a) => [UserId] -> Query (Accounts a) [a]
acctsFromIds ids =
    do accts <- liftM accountIxSet ask
       return $ map acctData $ toList (accts @+ ids)

acctsFromUsers :: (Data a, Ord a) => [Username] -> Query (Accounts a) [a]
acctsFromUsers names =
    do accts <- liftM accountIxSet ask
       return $ map acctData $ toList (accts @+ names)

acctFromId :: (Data a, Ord a) => UserId -> Query (Accounts a) (Maybe a)
acctFromId uid =
    do accts <- accountIxSet <$> ask
       return $ acctData <$> getOne (accts @= UserId)

changePassword :: (Data a, Ord a) =>
                  Username ->
                  String ->
                  Password ->
                  a ->
                  Update (Accounts a) (Either String ())
changePassword username oldPass newPw _ =
    do (Accounts nextId accountIxSet) <- get
       case (getOne (accountIxSet @= username)) of
         Nothing -> return (Left "No such user name")
         Just acct@(Account u i pw d) ->
             case checkPassword pw oldPass of
               True -> put (Accounts nextId (updateIx u (Account u i newPw d) accountIxSet)) >> return (Right ())
               False -> return (Left "Invalid password")

$(mkMethods ''Accounts
                [ 'authenticate
                , 'create
                , 'acctsFromIds
                , 'acctsFromUsers
                , 'acctFromId
                , 'changePassword
                ])

-- delete NOTE: if an account is deleted, and a new account is created
-- with the same usernanme, will the new user have access to stuff
-- from the old account? No, we user the UserId as the key.
