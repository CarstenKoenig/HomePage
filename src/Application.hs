{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Application
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH

import Network.Wai
import Network.Wai.Handler.Warp

import Servant
import Servant.HTML.Lucid (HTML)
import Servant.Utils.StaticFiles (serveDirectory)

import Lucid (Html)

import Views.Layout (Page, withLayout)
import Views.AboutMe
import Views.Index

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API
  = "users" :> Get '[JSON] [User]
  :<|> "static" :> Raw
  :<|> Get '[HTML] (Html ())


startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy


server :: Server API
server =
  return users
  :<|> serveDirectory "static"
  :<|> return (withLayout Views.AboutMe.page)


users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
