{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger (runNoLoggingT)
import           Control.Monad.Trans.Resource (runResourceT)

import           Crypto.Hash (SHA256)
import           Crypto.Cipher.Types (ctrCombine)
import           Crypto.Cipher.AES (AES256)
import           Crypto.Random (drgNew)

import           Data.Aeson (ToJSON(..),FromJSON(..))
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)

import           Database.Persist.Sql (runMigration, runSqlPool)
import           Database.Persist.Sqlite (withSqlitePool)

import           Servant.Server.Experimental.Auth.Cookie (mkRandomSource, mkServerKey)

import Application

import EventSourcing
import Database


hashPath :: FilePath
hashPath = "./hashes/"

localDb :: Text
localDb = "./database.db"


main :: IO ()
main = do
  let appPort = 8080
  randomSource <- mkRandomSource drgNew 2000
  serverKey    <- mkServerKey 16 (Just $ fromIntegral (86400 :: Integer))
  let authCookieSettings = AuthCookieSettings
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
        , baseUriPort      = appPort
        , baseUriScheme    = "http:"
        }

  -- using 5 database connections in pool
  runNoLoggingT $ withSqlitePool localDb 5 $ \ pool -> do
      let appContext =
            AppContext authSettings baseUri hashPath pool
      -- do database migration
      runSqlPool (runMigration migrateAll) pool
      -- and start the application
      liftIO $ startApp appContext

  where
    runEventStreamAction pool query =
      runResourceT . runNoLoggingT $
      runSqlPool (runStream query) pool

