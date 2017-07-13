{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE  DeriveGeneric    #-}

module Models.Events where

import           Data.Aeson (ToJSON(..),FromJSON(..), Value (..), (.=), (.:))
import qualified Data.Aeson as Json

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime)

import           Text.Markdown (Markdown(..))

import           GHC.Generics

import           EventSourcing.Events (Embedding(..))
import           Models.Blog.Types (BlogEvent)


data AppEvent
  = BlogEvent BlogEvent
  deriving (Eq, Show, Generic)


----------------------------------------------------------------------

instance ToJSON AppEvent where
    toEncoding = Json.genericToEncoding Json.defaultOptions

instance FromJSON AppEvent


instance Embedding AppEvent BlogEvent where
  embed = BlogEvent
  extract (BlogEvent ev) = Just ev
