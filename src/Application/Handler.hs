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
  ( AppHandler
  , appHandlerToHandler
  , addSessionFromContext
  , addSessionFromContextToError
  , AuthCookieSettings (..)
  , AppContext (..)
  , AuthSettings (..)
  , BaseUri (..)
  ) where

import Control.Monad.Catch (MonadThrow, try)
import Control.Monad.Except
import Control.Monad.Reader
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (ctrCombine)
import Crypto.Hash.Algorithms (SHA256)
import Crypto.Random
import Data.ByteString (ByteString)
import Data.Serialize hiding (Get)
import Data.Text (Text)
import Lucid
import Network.URI hiding (scheme)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Lucid
import Servant.Server.Experimental.Auth
import Servant.Server.Experimental.Auth.Cookie
import qualified Data.ByteString    as B
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T



-- | The handler monad, to work with cookies we need to have access to
-- 'AuthCookieSettings', 'RandomSource', and 'ServerKey'. It's also a handy
-- place to put some values that will be useful for route rendering.

type AppHandler = ReaderT AppContext (ExceptT ServantErr IO)


appHandlerToHandler :: AppContext -> AppHandler :~> Handler
appHandlerToHandler context =
  Nat $ flip runReaderT context  

-- | The application's context.
-- includes settings for the session encryption
-- and values needed to render routes

addSessionFromContext
  :: (AddHeader e EncryptedSession s r, Serialize a,
     MonadThrow m, MonadIO m)
  => AppContext -> a -> s -> m r
addSessionFromContext context =
  addSession
     (authCookieSettings $ appContextAuthSettings context)
     (authRandomSource $ appContextAuthSettings context)
     (authServerKey $ appContextAuthSettings context)


addSessionFromContextToError
  :: (MonadIO m, MonadThrow m, Serialize a) =>
     AppContext -> a -> ServantErr -> m ServantErr
addSessionFromContextToError context =
  addSessionToErr
     (authCookieSettings $ appContextAuthSettings context)
     (authRandomSource $ appContextAuthSettings context)
     (authServerKey $ appContextAuthSettings context)


data AppContext = AppContext
  { appContextAuthSettings :: AuthSettings
  , appContextBaseUri      :: BaseUri
  }


data AuthSettings = AuthSettings
  { authCookieSettings :: AuthCookieSettings
  , authRandomSource   :: RandomSource
  , authServerKey      :: ServerKey
  }


data BaseUri = BaseUri
  { baseUriRoot      :: String
  , baseUriPort      :: Int
  , baseUriScheme    :: String
  }
