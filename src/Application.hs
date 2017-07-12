{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module Application
    ( startApp
    , app
    , AuthCookieSettings (..)
    , AppContext (..)
    , AuthSettings (..)
    , BaseUri (..)
    ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (runReaderT, asks, ask, lift)
import           Control.Monad.Trans.Except (throwE, catchE)

import           Data.Aeson
import           Data.Aeson.TH
import           Data.ByteString (ByteString)
import           Data.Serialize (Serialize)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.Time.Clock (getCurrentTime)

import Network.URI (URIAuth (..), uriToString)
import Network.Wai
import Network.Wai.Handler.Warp

import Servant
import Servant.HTML.Lucid (HTML)
import Servant.Server.Experimental.Auth.Cookie (EncryptedSession, addSession)
import Servant.Utils.StaticFiles (serveDirectory)

import Lucid (Html)

import Application.Auth
import Application.Handler
import Application.Session

import Views.Layout (Page, PageContext(..),  withLayout)
import Views.AboutMe
import Views.Index

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type Routes
  = "static" :> Raw
  :<|> Pages

type Pages 
  = GetHomeR
  :<|> GetLoginR
  :<|> GetLogoutR


type GetHomeR =
  AppAuth :> Get '[HTML] (Html ())


type GetLoginR =
  "login" :>
  Get '[HTML] (Headers '[Header "set-cookie" EncryptedSession] (Html ()))


type GetLogoutR =
  AppAuth :>
  "logout" :>
  Get '[HTML] (Headers '[Header "set-cookie" EncryptedSession] (Html()))  


startApp :: AppContext -> IO ()
startApp context = do
  let port = baseUriPort (appContextBaseUri context)
  run port $ app context


app :: AppContext -> Application
-- app = serve api server
app appContext@AppContext {..} =
  serveWithContext
     (Proxy :: Proxy Routes)
     (
       (cookieAuthCheck
         (authCookieSettings appContextAuthSettings)
         (authServerKey appContextAuthSettings)
         :: AuthHandler Request (Maybe Session))
       :. EmptyContext)
     (server appContext)


server :: AppContext -> Server Routes
server context =
  serveDirectory "static"
  :<|> enter (appHandlerToHandler context) handlers


handlers :: ServerT Pages AppHandler    
handlers =
  homeHandler
  :<|> loginHandler
  :<|> logoutHandler


homeHandler :: Maybe Session -> AppHandler (Html ())
homeHandler =
  showPageHandler Views.AboutMe.page
  


loginHandler :: AppHandler (Headers '[Header "set-cookie" EncryptedSession] (Html ()))
loginHandler = do
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
  where getHomeUri = encodeUtf8 <$> routeToText (Proxy :: Proxy GetHomeR)
  
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
  :: Maybe Session          -- ^ Active 'Session' if any
  -> AppHandler PageContext -- ^ The resulting view
mkPageContext session = do
  homeLink   <- routeToText (Proxy :: Proxy GetHomeR)
  loginLink  <- routeToText (Proxy :: Proxy GetLoginR)
  logoutLink <- routeToText (Proxy :: Proxy GetLogoutR)
  return PageContext {..}


-- | Get textual representation for specific endpoint on the site.

routeToText :: (IsElem a Routes, HasLink a, MkLink a ~ URI)
  => Proxy a              -- ^ The 'Proxy' clarifying type of route to render
  -> AppHandler Text      -- ^ The rendered route as 'Text'
routeToText = renderURI . routeToURI

-- | Get link representation for specific endpoint on the site.

routeToURI :: (IsElem a Routes, HasLink a) => Proxy a -> MkLink a
routeToURI = safeLink (Proxy :: Proxy Routes)

-- | Render an 'URI' as 'Text'.

renderURI :: URI -> AppHandler Text
renderURI uri = do
  approot <- asks (baseUriRoot . appContextBaseUri)
  port    <- asks (baseUriPort . appContextBaseUri)
  scheme  <- asks (baseUriScheme . appContextBaseUri)
  let uri' = uri
        { uriScheme = scheme
        , uriAuthority = Just URIAuth
          { uriUserInfo = ""
          , uriRegName = approot
          , uriPort = ':' : show port }
        , uriPath = '/' : uriPath uri }
  (return . T.pack) (uriToString (const "") uri' "")
