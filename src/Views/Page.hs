{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Views.Page
  ( Page (..)
  , PageContext (..)
  ) where


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
  , homeLink    :: Text
  , loginLink   :: Text
  , logoutLink  :: Text
  }
