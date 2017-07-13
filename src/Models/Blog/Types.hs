{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE  DeriveGeneric    #-}

module Models.Blog.Types where

import           Data.Aeson (ToJSON(..),FromJSON(..), Value (..), (.=), (.:))
import qualified Data.Aeson as Json

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime)

import           Text.Markdown (Markdown(..))

import           GHC.Generics


data BlogEvent
  = ContentSet Markdown
  | TitleSet Text
  | Published UTCTime
  | AddedToCategory Category
  | RemovedFromCategory Category
  deriving (Eq, Show, Generic)


newtype Category
  = Category { categoryName :: Text }
  deriving (Eq, Show)



----------------------------------------------------------------------

instance ToJSON BlogEvent where
    toEncoding = Json.genericToEncoding Json.defaultOptions

instance FromJSON BlogEvent


instance FromJSON Category where
  parseJSON v = Category <$> parseJSON v

instance ToJSON Category where
  toJSON (Category text) = toJSON text


instance FromJSON Markdown where
  parseJSON (Object v) =
    Markdown <$> v .: "markdown"
  parseJSON _ = fail "expected a markdown object"

instance ToJSON Markdown where
  toJSON (Markdown text) = Json.object [ "markdown" .= text ]
