-- REMARK:
-- everything here is totally inspired (copied) from https://www.stackbuilders.com/tutorials/haskell/servant-auth/
-- cudos for all those hints

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application.Handler
  ( handlers
  ) where

import           Control.Monad.Catch (MonadThrow, try)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Except (throwE, catchE)

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Serialize hiding (Get)
import           Data.Text (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import           Data.Time (UTCTime, getCurrentTime, getCurrentTimeZone)

import           Lucid (Html)

import           Text.Markdown (Markdown(..))

import           Servant
import           Servant.HTML.Lucid
import           Servant.Server.Experimental.Auth
import           Servant.Server.Experimental.Auth.Cookie
import           System.Directory (doesFileExist)


import Application.HandlerMonad
import Application.Routing
import Application.Session (Session(..))
import Application.Context

import EventSourcing.EventStore (EventStream)

import Views.Layout
import Views.AboutMe
import Views.ShowBlogPost

import Models.Blog
import Models.Events (AppEvent)

import Database.Model (SqlQuery, runSqlQuery)
import Database.EventSourcing (runStream)

import qualified Utils.Passwords as Pw


handlers :: ServerT Pages AppHandler    
handlers =
  homeHandler
  :<|> blogHandler
  :<|> loginHandler
  :<|> logoutHandler


homeHandler :: Maybe Session -> AppHandler (Html ())
homeHandler =
  showPageHandler Views.AboutMe.page


blogHandler :: Maybe Session -> AppHandler (Html ())
blogHandler =
  showPageHandler (Views.ShowBlogPost.page demoPost)
  where
    demoPost = BlogPost (Markdown demoContent) "Demo Post" Nothing demoCats
    demoContent = "#Hello World\n\n- some\n- points"
    demoCats = [ Category "Demo" ]
  


loginHandler :: Pw.Login -> AppHandler (Headers '[Header "set-cookie" EncryptedSession] (Html ()))
loginHandler login = do
  hashPath <- asks appContextHashFilePath
  loginVerified <- liftIO $ Pw.verifyLogin hashPath login
  if loginVerified then do
    time <- liftIO getCurrentTime
    let session = Session time
    redirectHomeWithSession session
  else
    redirectHomeWithSession ()


-- | The “Sign Out” page sets cookies to empty byte string destroyng the data.

logoutHandler
  :: Maybe Session
  -> AppHandler (Headers '[Header "set-cookie" EncryptedSession] (Html ()))
logoutHandler ms =
  withSession ms $ \_ -> redirectHomeWithSession ()


redirectHomeWithSession
  :: (Serialize a)
  => a -> AppHandler (Headers '[Header "set-cookie" EncryptedSession] ret)
redirectHomeWithSession session = do
  redirectContext <- ask
  uri <- getHomeUri
  redirect <- addSessionFromContextToError
    redirectContext
    session
    (err303 { errHeaders = [("Location", uri)] })
  lift $ throwE redirect
  where getHomeUri = T.encodeUtf8 <$> routeToText (Proxy :: Proxy GetHomeR)
  
--------------------------------------------------------------------

-- | Perform actions with 'Session' or return 403 HTTP status code.

withSession
  :: Maybe Session     -- ^ 'Session', if any
  -> (Session -> AppHandler a) -- ^ Callback making use of 'Session'
  -> AppHandler a
withSession ms action = maybe (throwError err403) action ms


-- | run an computation on an Eventstream

runEventStream :: EventStream AppEvent res -> AppHandler res
runEventStream = runSql . runStream


runSql :: SqlQuery res -> AppHandler res
runSql query = do
  pool <- asks appContextSqlPool
  liftIO $ runSqlQuery pool query


-- | shows a page
showPageHandler :: (PageContext Session -> Page) -> Maybe Session -> AppHandler (Html ())
showPageHandler page session = do
  context <- mkPageContext session
  return (withLayout context page)


-- | Create an 'PageContext' this is the recommended method to created it

mkPageContext
  :: Maybe s                    -- ^ Active 'Session' if any
  -> AppHandler (PageContext s) -- ^ The resulting view
mkPageContext session = do
  timezone   <- liftIO getCurrentTimeZone
  homeLink   <- routeToText (Proxy :: Proxy GetHomeR)
  loginLink  <- routeToText (Proxy :: Proxy GetLoginR)
  logoutLink <- routeToText (Proxy :: Proxy GetLogoutR)
  return PageContext {..}


