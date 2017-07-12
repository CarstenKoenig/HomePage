{-# LANGUAGE OverloadedStrings #-}
module Utils.Passwords
  ( Password
  , Salt
  , PasswordHash
  , Login (..)
  , verifyLogin
  , verifyPassword
  , createPassword
  , createPasswordHash
  , newSalt128
  , writePasswordHashToFile
  , writeNewPasswordHashToFile
  , readPasswordHashFromFile
  )where


import           Crypto.Hash (Digest, SHA256(..), hash)
import           Crypto.Random (drgNew, withRandomBytes)

import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BA
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Char (toLower)
import           Data.Serialize (Serialize(..))
import qualified Data.Serialize as Ser
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)

import           System.Directory (doesFileExist)

import           Web.Internal.FormUrlEncoded (FromForm(..), parseUnique)


newtype Password =
  Password { getPwd :: Text }


newtype Salt =
  Salt128 { getSalt128 :: ByteString }
  deriving (Eq, Show)


data PasswordHash =
  PasswordHash { hashSalt :: Salt
               , getHash :: ByteString
               }
  deriving Eq


data Login =
  Login { user     :: Text
        , password :: Password
        }

----------------------------------------------------------------------

createPassword :: Text -> Password
createPassword = Password . T.map toLower


createPasswordHash :: Salt -> Password -> PasswordHash
createPasswordHash salt =
  PasswordHash salt .
  asBytestring . (hash :: ByteString -> Digest SHA256) .
  encodeUtf8 . getPwd
  where
    asBytestring = BA.convertToBase BA.Base16


verifyPassword :: PasswordHash -> Password -> Bool
verifyPassword hash pwd =
  hash == createPasswordHash (hashSalt hash) pwd


----------------------------------------------------------------------

verifyLogin :: FilePath -> Login -> IO Bool
verifyLogin path login = do
  putStr $ "loging in " ++ show (user login)
  let file = path ++ T.unpack (user login) ++ ".hash"
  exists <- doesFileExist file
  if not exists
    then do
      putStrLn ".. unkown user"
      return False
    else do
      ph <- readPasswordHashFromFile file
      if verifyPassword ph (password login) then do
        putStrLn ".. OK"
        return True
      else do
        putStrLn ".. wrong Password"
        return False


readPasswordHashFromFile :: FilePath -> IO PasswordHash
readPasswordHashFromFile path = do
  decoded <- Ser.decode <$> B.readFile path
  case decoded of
    Left err -> error err
    Right ph -> return ph


writeNewPasswordHashToFile :: FilePath -> Text -> IO ()
writeNewPasswordHashToFile path password = do
  salt <- newSalt128
  B.writeFile path . Ser.encode
    $ createPasswordHash salt (createPassword password)


writePasswordHashToFile :: FilePath -> PasswordHash -> IO ()
writePasswordHashToFile path =
  B.writeFile path . Ser.encode
  

newSalt128 :: IO Salt
newSalt128 = do
  rnd <- drgNew
  return . Salt128 . fst $ withRandomBytes rnd 128 getBs
  where
    getBs :: ByteString -> ByteString
    getBs = BA.convertToBase BA.Base16


----------------------------------------------------------------------

instance IsString Password where
  fromString = Password . fromString


instance FromForm Login where
  --fromFormUrlEncoded :: [(Text, Text)] -> Either String CheckRequest
  fromForm f = Login
    <$> parseUnique "user" f
    <*> (createPassword <$> parseUnique "password" f)


instance Serialize PasswordHash where
  put (PasswordHash (Salt128 salt) hash) = do
    put salt
    put hash
  get = do
    salt <- Salt128 <$> get
    hash <- get
    return $ PasswordHash salt hash
  
