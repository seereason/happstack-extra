{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
module Happstack.Server.Account.Server
    ( signInDirName
    , signOutDirName
    , signUpDirName
    , account
    ) where

import Control.Monad(msum)
import Control.Monad.State ()
import Control.Monad.Trans (lift, MonadIO(liftIO))
--import Data.Generics (Data(..))
import Extra.URI (URI(..), parseURI, parseRelativeReference, relURI, setURIQueryAttr, unEscapeString)
import HSP.XML (XML)
import HSP (HSP, evalHSP)
import Happstack.Data (Default(..))
import Happstack.Data.User.Password (newPassword)
import Happstack.Server (Method(GET, POST), WebT(..), ServerPartT(..), Response,
                     dir, method, ok, toResponse, withDataFn,
                     seeOther, look, anyRequest,
                     mkCookie, addCookie, readCookieValue, methodSP)
import Happstack.Server.Account(AccountData, Create(..), Account(..), UserId(..), Username(..), Authenticate(..))
import Happstack.Server.Extra (lookPairsUnicode, withURI, withURISP)
import Happstack.Server.HSP.HTML()
import Happstack.Server.Session(SessionData, SessionId(..), Session(..), GetSession(..), DelSession(..), newSession)
import Happstack.State (query, update)

-- |Try to authenticate a user and password, and on success (creation
-- of the new session) return the account data and session ID.
llogin :: (MonadIO m, AccountData a, SessionData s) =>
          (String -> UserId -> a -> s) -> String -> String -> WebT m (Maybe (Account a), Maybe SessionId)
llogin makeSess u p = 
    do a <- query (Authenticate u p)
       case a of
         Just account {- (Account _userId _ _ (AccountData greet)) -} ->
              do sId <- newSession (makeSess u (userId account) (acctData account))
                 addCookie (-1) (mkCookie "sessionId" (show sId))
                 return (a, Just sId)
         Nothing -> return (Nothing, Nothing)

account :: forall acct. forall sess.
           (AccountData acct, SessionData sess) =>
           (URI -> sess -> HSP XML)                     -- ^ Create the login page with the given destination URI
        -> (String -> UserId -> acct -> sess)           -- ^ Create a session
        -> (SessionId -> DelSession sess)               -- ^ Delete a session
        -> String                                       -- ^ The path to the parent serverpart, e.g. "/account"
        -> [ServerPartT IO Response]
account logInPage makeSess delSess path =
    [withDataFn lookPairsUnicode $ \ pairs ->
      -- We expect to see dest=<encoded uri> in the query, that is the
      -- page we go to once we log in successfully.
     let destString = fmap unEscapeString (lookup "dest" pairs)
         destURI = maybe Nothing (\ s -> maybe (parseURI s) Just (parseRelativeReference s)) destString
         dest = maybe (relURI path []) id destURI
         alert = lookup "alert" pairs in
      msum
      [ handleSignUp makeSess (defaultValue :: acct) dest alert
      , handleSignIn makeSess
      , withDataFn (readCookieValue "sessionId") $ \ sID -> msum
        [ handleSignOut delSess dest sID
        -- Add pages that need a session
        , haveSession logInPage dest sID ]
      -- This is where you end up if you're not logged in.
      -- Pass in an inactive session.
      , noSession logInPage dest
      ]
    ]

signUpDirName = "signUp"

-- |A server part that handles the /account/signUp form action and
-- tries to create a new account.
handleSignUp :: (MonadIO m, AccountData a, SessionData s) =>
                (String -> UserId -> a -> s) -> a -> URI -> Maybe String -> ServerPartT m Response
handleSignUp makeSess defAcct dest alert =
    dir signUpDirName $
      methodSP POST $
        (withDataFn (do u  <- look "newusername"
                        p1 <- look "newpassword1"
                        p2 <- look "newpassword2"
                        return (u, p1, p2)
                    ) $ \(u, p1, p2) ->
        withURI $ \ here ->
           (if p1 /= p2
            then ok (toResponse "Passwords do not match.  Press the 'back' button and try again.")
            else do pw <- liftIO (newPassword p1)
                    -- FIXME: This should have more error conditions
                    r <- update $ Create (Username u) pw defAcct
                    case r of
                      Left error -> seeOther (setURIQueryAttr "dest" (show dest) (setURIQueryAttr "alert" error here)) (toResponse error)
                      Right _userId -> do (a,_sid) <- llogin makeSess u p1
                                          case a of
                                            Nothing -> ok (toResponse "Authentication failed.")
                                            _ -> seeOther dest (toResponse ()))
        )

signInDirName = "signIn"

handleSignIn :: (MonadIO m, AccountData a, SessionData s) =>
          (String -> UserId -> a -> s) -> ServerPartT m Response
handleSignIn makeSess =
    dir signInDirName $
      methodSP POST $
        withURISP $ \here ->
        [ withDataFn (do u <- (look "username")
                         p <- (look "password")
                         dest <- look "dest"
                         return (u, p, maybe here id (parseRelativeReference dest))
                     ) $ \(u, p, dest) ->
          anyRequest $
            do (a,_sid) <- llogin makeSess u p
               case a of
                 Nothing -> seeOther (setURIQueryAttr "alert" "Authentication failed." dest) (toResponse ()) -- ok (toResponse "Authentication failed.")
                 _ -> seeOther dest (toResponse ())
        ]

signOutDirName = "signOut"

handleSignOut :: (MonadIO m, SessionData sess) =>
           (SessionId -> DelSession sess) -> URI -> SessionId -> ServerPartT m Response
handleSignOut delSess dest sID =
    withDataFn (readCookieValue "sessionId") $ \sID ->
      dir signOutDirName $
        anyRequest $ do update (delSess sID)
                        seeOther dest (toResponse ())

haveSession :: (SessionData sess) =>
               (URI -> sess -> HSP XML) -> URI -> SessionId -> ServerPartT IO Response
haveSession logInPage dest sID =
    method GET $
           do mSessData <- query (GetSession sID)
              let sessData =
                      case mSessData of
                        Just (Session _ sessionData) -> sessionData
                        Nothing -> defaultValue -- expired session might be better
              ok . toResponse =<< lift (evalHSP Nothing (logInPage dest sessData))

noSession :: (SessionData sess) =>
             (URI -> sess -> HSP XML) -> URI -> ServerPartT IO Response
noSession logInPage dest =
    method GET $ ok . toResponse =<< lift (evalHSP Nothing (logInPage dest defaultValue))
