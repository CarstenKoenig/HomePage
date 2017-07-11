{-# LANGUAGE OverloadedStrings #-}

module Views.Index where

import           Data.Text (Text)

import           Lucid (Html, toHtml, html_)
import qualified Lucid as Html


page :: Html ()
page =
  html_ $
  Html.body_ $ do
  Html.h1_ [Html.class_ "heading"] (toHtml ("Hello Web" :: Text))
