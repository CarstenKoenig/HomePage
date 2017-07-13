{-# LANGUAGE OverloadedStrings #-}

module Models.Blog.Post
  ( BlogPost (..)
  , blogPostP
  ) where

import           Data.Aeson (ToJSON(..),FromJSON(..), Value (..), (.=), (.:))
import qualified Data.Aeson as Json

import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime)
import           Data.Set (Set)
import qualified Data.Set as S

import           Text.Markdown (Markdown(..))


import Models.Blog.Types
import EventSourcing.Projections


data BlogPost =
  BlogPost { blogPostContent       :: Markdown
           , blogPostTitle         :: Text
           , blogPostPublishedTime :: Maybe UTCTime
           , blogPostCategories    :: [Category]
           }


----------------------------------------------------------------------

blogPostP :: Projection BlogEvent BlogPost
blogPostP =
  BlogPost <$> contentP <*> titleP <*> publishedP <*> categoriesP


contentP :: Projection BlogEvent Markdown
contentP = fromMaybe noContent <$> lastP selectContent
  where
    selectContent (ContentSet md) = Just md
    selectContent _               = Nothing
    noContent = Markdown ""


titleP :: Projection BlogEvent Text
titleP = fromMaybe "" <$> lastP selectTitle
  where
    selectTitle (TitleSet t) = Just t
    selectTitle _            = Nothing


publishedP :: Projection BlogEvent (Maybe UTCTime)
publishedP = lastP selectPublished
  where
    selectPublished (Published t) = Just t
    selectPublished _             = Nothing


categoriesP :: Projection BlogEvent [Category]
categoriesP = map Category . S.toAscList . foldr ($) S.empty <$> collectP catOps
  where
    catOps (AddedToCategory (Category c))     = Just (S.insert c)
    catOps (RemovedFromCategory (Category c)) = Just (S.delete c)
    catOps _                                  = Nothing
    
