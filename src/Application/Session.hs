-- REMARK:
-- everything here is totally inspired (copied) from https://www.stackbuilders.com/tutorials/haskell/servant-auth/
-- cudos for all those hints

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application.Session where

import Control.Monad.Catch (try)
import Control.Monad.Except
import Control.Monad.Reader
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (ctrCombine)
import Crypto.Hash.Algorithms (SHA256)
import Crypto.Random
import Data.ByteString (ByteString)
import Data.Serialize hiding (Get)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
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


-- | A 'Session' here for now just includes the time when the session was created

data Session = Session
  { sessionStart :: UTCTime
  }


-- In order to store the 'Session' in a cookie, we need to make it an
-- instance of the 'Serialize' type class.

instance Serialize Session where
  put (Session time) =
    put (toRational $ utcTimeToPOSIXSeconds time)
  get = do
    time <- posixSecondsToUTCTime . fromRational <$> get
    return (Session time)  
