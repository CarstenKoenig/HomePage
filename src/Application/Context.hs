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

import Servant.Server.Experimental.Auth.Cookie


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
