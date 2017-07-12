{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
module Application.Routing where

import           Control.Monad.Reader (runReaderT, asks, ask, lift)

import           Data.Text (Text)
import qualified Data.Text as T

import           Network.URI (URIAuth (..), uriToString)

import           Servant
import           Servant.HTML.Lucid (HTML)
import           Servant.Server.Experimental.Auth.Cookie (EncryptedSession, addSession)
import           Servant.Utils.StaticFiles (serveDirectory)
import           Web.Internal.FormUrlEncoded (FromForm(..), parseUnique)

import           Lucid (Html)

import           Application.Auth (AppAuth)
import           Application.HandlerMonad (AppHandler)
import           Application.Context
import           Application.Session (Session(..))



type Routes
  = "static" :> Raw
  :<|> Pages


type Pages 
  = GetHomeR
  :<|> GetLoginR
  :<|> GetLogoutR


type GetHomeR =
  AppAuth :> Get '[HTML] (Html ())


data Login =
  Login { user :: Text
        , password :: Text
        }
  deriving (Show)


instance FromForm Login where
  --fromFormUrlEncoded :: [(Text, Text)] -> Either String CheckRequest
  fromForm f =
    Login <$> parseUnique "user" f <*> parseUnique "password" f


type GetLoginR =
  "login" :>
  ReqBody '[FormUrlEncoded] Login :>
  Post '[HTML] (Headers '[Header "set-cookie" EncryptedSession] (Html ()))


type GetLogoutR =
  AppAuth :>
  "logout" :>
  Get '[HTML] (Headers '[Header "set-cookie" EncryptedSession] (Html()))  


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
