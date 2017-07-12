-- REMARK:
-- everything here is totally inspired (copied) from https://www.stackbuilders.com/tutorials/haskell/servant-auth/
-- cudos for all those hints

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application.Auth
  ( AuthHandler
  , AppAuth
  , cookieAuthCheck
  ) where

import Control.Monad.Catch (try)
import Control.Monad.Except (ExceptT)
import Network.Wai (Request)

import Servant
import Servant.Server.Experimental.Auth
import Servant.Server.Experimental.Auth.Cookie


import Application.Session


type instance AuthCookieData = Maybe Session

-- | The acutal session check. The ‘servant-auth-cookie’ package provides
-- the 'defaultAuthHandler', but that does not indicate missing 'Session' as
-- 'Nothing', so we use the custom one.

cookieAuthCheck
  :: AuthCookieSettings
  -> ServerKey
  -> AuthHandler Request (Maybe Session)
cookieAuthCheck authSettings serverKey =
  mkAuthHandler $ \request -> do
      result <- try (getSession authSettings serverKey request)
      case result :: Either AuthCookieException (Maybe Session) of
        Left _        -> return Nothing
        Right session -> return session


-- | It's generally a good idea to have a type synonym for your
-- authentication type so it's easier to modify it later.

type AppAuth = AuthProtect "cookie-auth"

-- This orphan instance is necessary in order to teach Servant how to render
-- routes that use our authentication method.

instance HasLink sub => HasLink (AppAuth :> sub) where
  type MkLink (AppAuth :> sub) = MkLink sub
  toLink _ = toLink (Proxy :: Proxy sub)
