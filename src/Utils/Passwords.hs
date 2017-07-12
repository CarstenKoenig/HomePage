{-# LANGUAGE OverloadedStrings #-}
module Utils.Passwords
  ( Password (Password)
  , Salt
  , PasswordHash
  , verifyPassword
  , createPasswordHash
  , newSalt
  , writePasswordHashToFile
  , writeNewPasswordHashToFile
  , readPasswordHashFromFile
  )where


import           Data.Serialize (Serialize(..))
import qualified Data.Serialize as Ser
import           Data.String (IsString(..))

import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)

import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Crypto.Hash (Digest, SHA256(..), hash)
import Crypto.Random (drgNew, withRandomBytes)


saltLength :: Int
saltLength = 128

newtype Password =
  Password { getPwd :: Text }


instance IsString Password where
  fromString = Password . fromString


newtype Salt =
  Salt { getSalt :: ByteString }
  deriving (Eq, Show)


data PasswordHash =
  PasswordHash { hashSalt :: Salt
               , getHash :: ByteString
               }
  deriving Eq


newSalt :: IO Salt
newSalt = do
  rnd <- drgNew
  return . Salt . fst $ withRandomBytes rnd saltLength getBs
  where
    getBs :: ByteString -> ByteString
    getBs = BA.convertToBase BA.Base16


verifyPassword :: PasswordHash -> Password -> Bool
verifyPassword hash pwd =
  hash == createPasswordHash (hashSalt hash) pwd


createPasswordHash :: Salt -> Password -> PasswordHash
createPasswordHash salt =
  PasswordHash salt .
  asBytestring . (hash :: ByteString -> Digest SHA256) .
  encodeUtf8 . getPwd
  where
    asBytestring = BA.convertToBase BA.Base16


writeNewPasswordHashToFile :: FilePath -> Salt -> Password -> IO ()
writeNewPasswordHashToFile path salt =
  B.writeFile path . Ser.encode .  createPasswordHash salt


writePasswordHashToFile :: FilePath -> PasswordHash -> IO ()
writePasswordHashToFile path =
  B.writeFile path . Ser.encode
  

readPasswordHashFromFile :: FilePath -> IO PasswordHash
readPasswordHashFromFile path = do
  decoded <- Ser.decode <$> B.readFile path
  case decoded of
    Left err -> error err
    Right ph -> return ph


instance Serialize PasswordHash where
  put (PasswordHash (Salt salt) hash) = do
    put salt
    put hash
  get = do
    salt <- Salt <$> get
    hash <- get
    return $ PasswordHash salt hash
  
