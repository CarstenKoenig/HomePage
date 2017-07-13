{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application.Context
  ( AuthCookieSettings (..)
  , AppContext (..)
  , AuthSettings (..)
  , BaseUri (..)
  ) where


import Data.Aeson (FromJSON, ToJSON)
import Servant.Server.Experimental.Auth.Cookie

import EventSourcing
import Models.Events (AppEvent)


-- | The application's context.
-- includes settings for the session encryption
-- and values needed to render routes

data AppContext = AppContext
  { appContextAuthSettings   :: AuthSettings
  , appContextBaseUri        :: BaseUri
  , appContextHashFilePath   :: FilePath
  , appContextRunEventStream :: forall res . EventStream AppEvent res -> IO res
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
