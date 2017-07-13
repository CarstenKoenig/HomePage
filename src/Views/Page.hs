{-# LANGUAGE OverloadedStrings #-}

module Views.Page
  ( Page (..)
  , PageContext (..)
  ) where


import Data.Time (TimeZone)
import Data.Text (Text)
import Lucid (Html)


data Page =
  Page { additionalStyles :: Maybe Text
       , title       :: Text
       , content     :: Html ()
       }


data PageContext s =
  PageContext
  { session     :: Maybe s
  , timezone    :: TimeZone
  , homeLink    :: Text
  , loginLink   :: Text
  , logoutLink  :: Text
  }
