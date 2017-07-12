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

import           Data.Serialize hiding (Get)
import           Data.Text (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import           Data.Time (UTCTime, getCurrentTime)

import Lucid (Html)
import Servant
import Servant.HTML.Lucid
import Servant.Server.Experimental.Auth
import Servant.Server.Experimental.Auth.Cookie

import Application.HandlerMonad
import Application.Routing
import Application.Session (Session(..))

import Views.Layout
import Views.AboutMe


handlers :: ServerT Pages AppHandler    
handlers =
  homeHandler
  :<|> loginHandler
  :<|> logoutHandler


homeHandler :: Maybe Session -> AppHandler (Html ())
homeHandler =
  showPageHandler Views.AboutMe.page
  


loginHandler :: Login -> AppHandler (Headers '[Header "set-cookie" EncryptedSession] (Html ()))
loginHandler login = do
  liftIO $ putStrLn $ "loging in " ++ show login
  time <- liftIO getCurrentTime
  let session = Session time
  redirectHomeWithSession session
  


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


-- | shows a page
showPageHandler :: Page -> Maybe Session -> AppHandler (Html ())
showPageHandler page session = do
  context <- mkPageContext session
  return (withLayout context page)


-- | Create an 'PageContext' this is the recommended method to created it

mkPageContext
  :: Maybe s                    -- ^ Active 'Session' if any
  -> AppHandler (PageContext s) -- ^ The resulting view
mkPageContext session = do
  homeLink   <- routeToText (Proxy :: Proxy GetHomeR)
  loginLink  <- routeToText (Proxy :: Proxy GetLoginR)
  logoutLink <- routeToText (Proxy :: Proxy GetLogoutR)
  return PageContext {..}


