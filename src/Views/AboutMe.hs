{-# LANGUAGE OverloadedStrings #-}
module Views.AboutMe
  ( page
  ) where

import Data.Default (def)
import Data.Text (Text)
import qualified Data.Text as T

import Control.Monad.IO.Class (MonadIO)

import Lucid (Html, toHtml)
import qualified Lucid.Html5 as H
import qualified Lucid.Bootstrap as BS

import Views.Layout (Page (Page))


page :: Page
page = Page Nothing "about me" content


content :: Html ()
content = do
  H.div_ [ H.id_ "aboutMe"] $ do
    BS.row_ $ do
      col_ 2 ""
      col_ 4 $ do
        H.div_ [ H.class_ "caption" ] $ do
          H.h2_ [ H.class_ "text-right" ] "Carsten König"
      col_ 6 ""

    BS.row_ $ do
      col_ 2 ""
      col_ 4 $ do
        H.div_ [ H.class_ "thumbnail" ] $ do
          H.img_ [ H.src_ "static/images/carsten.jpg", H.alt_ "Carsten König" ]
      col_ 4 $ do
        H.dl_ [ H.class_ "dl-horizontal" ] $ do
          H.dt_ "email:"
          H.dd_ $ H.a_ [ H.href_ "mailto://Carsten.Koenig@hotmail.de" ] "Carsten.Koenig@hotmail.de"
          
          H.dt_ "old blog:"
          H.dd_ $ H.a_ [ H.href_ "http://gettingsharpder.de" ] "getting#er"

        
          H.dt_ "languages:"
          H.dd_ $ H.p_ $ do
            H.span_ [ H.class_ "label label-primary"] "F#"
            H.span_ [ H.class_ "label label-primary"] "C#"
            H.span_ [ H.class_ "label label-primary"] "Haskell"
            H.span_ [ H.class_ "label label-primary"] "Idris"
            H.span_ [ H.class_ "label label-primary"] "Elm"
            H.span_ [ H.class_ "label label-primary"] "PureScript"
    

col_ :: Int -> Html a -> Html a
col_ n = H.div_ [ H.class_ . T.pack $ "col-md-" ++ show n ]


jumbotron_ :: Html a -> Html a
jumbotron_ = H.div_ [ H.class_ "jumbotron" ]
