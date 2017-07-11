{-# LANGUAGE OverloadedStrings #-}
module Views.Layout
  ( Page (..)
  , withLayout
  ) where


import Data.Default
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Control.Monad
import Control.Monad.IO.Class (MonadIO)

import Lucid (Html, Attribute, toHtml, renderText)
import qualified Lucid.Html5 as H
import qualified Lucid.Bootstrap as BS

import Control.Monad (forM_)


data Page =
  Page { additionalStyles :: Maybe Text
       , title :: Text
       , content :: Html ()
       }


withLayout :: Page -> Html ()
withLayout page = do
  H.doctype_ 
  H.html_ [ H.lang_ "de" ] $ do
    H.head_ $ do
      H.link_ [ H.rel_ "shortcut icon", H.href_ "static/favicon.ico" ]
      H.meta_ [ H.charset_ "utf-8" ]
      H.meta_ [ H.name_ "description"
              , H.content_ "functional programming and more" ]
      H.meta_ [ H.name_ "author"
              , H.content_ "Carsten König" ]
      H.meta_ [ H.httpEquiv_ "X-UA-Compatible"
              , H.content_ "IE=edge" ]
      H.meta_ [ H.name_ "viewport"
              , H.content_ "width=device-width, initial-scale=1" ]
        
      H.title_ $ toHtml $ title page
      
      -- Bootstrap
      H.link_ [ H.href_ "static/css/bootstrap.min.css"
              , H.rel_ "stylesheet" ]
      -- Custom css
      H.link_ [ H.href_ "static/css/site.css"
              , H.rel_ "stylesheet" ]

      H.style_ $ fromMaybe "" $ additionalStyles page
        
      H.body_ $ do

        H.div_ [ H.class_ "blog-masthead" ] $ do
          BS.container_ $ nav
      
        BS.container_ $ do
          H.div_ [ H.id_ "main", H.role_ "main" ] $ content page

        H.footer_ [ H.class_ "blog-footer" ] $ do
          H.div_ [ H.class_ "col-md-2 text-left" ] $ do
            H.p_ $ do
              H.a_ [ H.href_ "#" ] "nach oben"
          H.div_ [ H.class_ "col-md-8" ] ""

        -- JQuery
        H.script_
          [ H.src_ "static/js/jquery.min.js" ]
          T.empty
        -- Bootstrap
        H.script_ [ H.src_ "static/js/bootstrap.min.js" ] T.empty
        -- MathJax
        H.script_
          [ H.src_ "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-AMS_HTML" ]
          T.empty


nav :: Html ()
nav = do
  H.nav_ [ H.class_ "blog-nav" ] $ do
     forM_ [ ("Home", "/") ] $ navItem


navItem :: (Text,Text) -> Html ()
navItem (txt,ref) =
    H.a_ [ H.class_ "blog-nav-item", H.href_ ref ] (toHtml txt)
    

jumbotron_ :: Html a -> Html a
jumbotron_ = H.div_ [ H.class_ "jumbotron" ]
