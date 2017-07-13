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


data AppContext = AppContext
  { appContextAuthSettings   :: AuthSettings
  , appContextBaseUri        :: BaseUri
  , appContextHashFilePath   :: FilePath
  , appContextRunEventStream :: forall ev res . (FromJSON ev, ToJSON ev)
                             => EventStream ev res -> IO res
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
