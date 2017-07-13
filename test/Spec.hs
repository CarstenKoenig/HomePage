{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Crypto.Hash (SHA256)
import           Crypto.Cipher.AES (AES256)
import           Crypto.Cipher.Types (ctrCombine)
import           Data.Proxy(Proxy(..))
import           Crypto.Random (drgNew)

import           Servant.Server.Experimental.Auth.Cookie (mkRandomSource, mkServerKey)


import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON


import Application (app)
import Application.Context


main :: IO ()
main = do
  context <- testContext
  hspec $ spec context

spec :: AppContext -> Spec
spec context = with (return $ app context) $
  describe "GET /" $
  it "responds with 200" $
  get "/" `shouldRespondWith` 200


testContext :: IO AppContext
testContext = do
  randomSource <- mkRandomSource drgNew 2000
  serverKey    <- mkServerKey 16 (Just $ fromIntegral (86400 :: Integer))
  let authCookieSettings =
        AuthCookieSettings
        { acsSessionField = "http://carsten-koenig.net/session-cookie"
        , acsCookieFlags  = ["HttpOnly"]
        , acsMaxAge       = fromIntegral (6 * 3600 :: Integer)
        , acsExpirationFormat = "%0Y%m%d%H%M%S"
        , acsPath         = "/"
        , acsHashAlgorithm = Proxy :: Proxy SHA256
        , acsCipher       = Proxy :: Proxy AES256
        , acsEncryptAlgorithm = ctrCombine
        , acsDecryptAlgorithm = ctrCombine
        }
      authSettings = AuthSettings
        { authCookieSettings = authCookieSettings
        , authRandomSource   = randomSource
        , authServerKey      = serverKey
        }
      baseUri = BaseUri
        { baseUriRoot      = "localhost"
        , baseUriPort      = 80
        , baseUriScheme    = "http:"
        }

  return $ AppContext authSettings baseUri "." undefined
