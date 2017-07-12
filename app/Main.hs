{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Crypto.Hash (SHA256)
import           Crypto.Cipher.Types (ctrCombine)
import           Crypto.Cipher.AES (AES256)
import           Crypto.Random (drgNew)

import           Data.Aeson (ToJSON(..),FromJSON(..))
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)

import           Database.Persist.Sql (runMigration)
import           Database.Persist.Sqlite (runSqlite)

import           Servant.Server.Experimental.Auth.Cookie (mkRandomSource, mkServerKey)

import Application

import EventSourcing
import Database

main :: IO ()
main = do
  let appPort = 8080
  randomSource <- mkRandomSource drgNew 2000
  serverKey    <- mkServerKey 16 (Just $ fromIntegral (86400 :: Integer))
  let authCookieSettings = AuthCookieSettings
        { acsSessionField = "Session"
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
        , baseUriPort      = appPort
        , baseUriScheme    = "http:"
        }
      appContext = AppContext authSettings baseUri
  startApp appContext


runInSqlite :: (ToJSON ev, FromJSON ev) => Text -> EventStream ev res -> IO res
runInSqlite connection query = do
  runSqlite connection $ do
    runMigration migrateAll
    runStream query


dbConnection :: Text
dbConnection = "events.db"

