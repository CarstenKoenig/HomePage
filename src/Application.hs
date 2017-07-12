{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
module Application
    ( startApp
    , AuthCookieSettings (..)
    , AppContext (..)
    , AuthSettings (..)
    , BaseUri (..)
    ) where


import Network.Wai
import Network.Wai.Handler.Warp

import Servant

import Application.Auth (AuthHandler, cookieAuthCheck)
import Application.Context
import Application.Handler (handlers)
import Application.HandlerMonad (appHandlerToHandler)
import Application.Session (Session)
import Application.Routing (Routes)


startApp :: AppContext -> IO ()
startApp context = do
  let port = baseUriPort (appContextBaseUri context)
  run port $ app context


app :: AppContext -> Application
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
