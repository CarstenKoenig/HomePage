{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Views.Layout
  ( Page (..)
  , PageContext (..)
  , withLayout
  ) where


import           Data.Default
import           Data.Maybe (fromMaybe, isNothing)
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO)

import Lucid (Html, Attribute, toHtml, renderText)
import Lucid.Base (makeAttribute)
import qualified Lucid.Html5 as H
import qualified Lucid.Bootstrap as BS


import Views.Page
import qualified Views.LoginModal as Login


withLayout :: PageContext s -> Page -> Html ()
withLayout context page = do
  H.doctype_ 
  H.html_ [ H.lang_ "de" ] $
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

        when (isNothing $ session context) (Login.render context "DlgLogin")

        H.div_ [ H.class_ "blog-masthead" ] $
          BS.container_ $ nav context
      
        BS.container_ $
          H.div_ [ H.id_ "main", H.role_ "main" ] $ content page

        H.footer_ [ H.class_ "blog-footer" ] $ do
          H.div_ [ H.class_ "col-md-2 text-left" ] $
            H.p_ $
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


nav :: PageContext s -> Html ()
nav context =
  H.nav_ [ H.class_ "blog-nav" ] $
     sequence_ (links context)

links :: PageContext s -> [Html ()]
links context =
  case session context of
    Nothing -> [ navItem ("Home", homeLink context)
               , loginButton
               ]
    Just _  -> [ navItem ("Home", homeLink context)
               , navItem ("Logout", logoutLink context)
               ]


loginButton :: Html ()
loginButton =
  H.a_ [ H.class_ "blog-nav-item"
       , data_ "toggle" "modal"
       , data_ "target" "#DlgLogin"
       ]
    (toHtml ("Login" :: Text))


data_ :: Text -> Text -> Attribute
data_ tag = attr_ (T.append "data-" tag)


attr_ :: Text -> Text -> Attribute
attr_ = makeAttribute


navItem :: (Text,Text) -> Html ()
navItem (txt,ref) =
    H.a_ [ H.class_ "blog-nav-item", H.href_ ref ] (toHtml txt)
    

jumbotron_ :: Html a -> Html a
jumbotron_ = H.div_ [ H.class_ "jumbotron" ]
