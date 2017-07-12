{-# LANGUAGE OverloadedStrings #-}
module Views.LoginModal
  ( render
  ) where

import Data.Default (def)
import Data.Text (Text)
import qualified Data.Text as T

import Control.Monad.IO.Class (MonadIO)

import Lucid (Html, Attribute, toHtml, toHtmlRaw)
import Lucid.Base (makeAttribute)
import qualified Lucid.Html5 as H
import qualified Lucid.Bootstrap as BS


import Views.Page (PageContext(..))


render :: PageContext s -> Text -> Html ()
render context modalId =
  H.div_ [ H.classes_ ["modal", "fade"]
         , H.id_ modalId
         , H.tabindex_ "-1"
         , H.role_ "dialog"
         , aria_ "labelledby" labelId
         ] $
    H.div_ [ H.class_ "modal-dialog"
           , H.role_ "document"
           ] $
      H.div_ [ H.class_ "modal-content" ] $ do
        H.div_ [ H.class_ "modal-header"] $ do
          H.button_ [ H.type_ "button"
                    , H.class_ "close"
                    , data_ "dismiss" "modal"
                    , aria_ "label" "Close"
                    ] $
            H.span_ [ aria_ "hidden" "true" ] (toHtmlRaw ("&times;" :: Text))
          H.h4_ [ H.class_ "modal-title", H.id_ labelId ] (toHtml ("Login" :: Text))
        H.div_ [ H.class_ "modal-body"] (body $ loginLink context)
  where
    labelId = T.append modalId "Label"


body :: Text -> Html ()
body action = 
  H.form_ [ H.class_ "form-inline"
          , H.action_ action
          , H.method_ "POST"
          ] $ do
    H.div_ [ H.class_ "form-group" ] $ do
      H.label_ [ H.class_ "sr-only", H.for_ "user"] (toHtml ("user" :: Text))
      H.input_ [ H.type_ "text"
               , H.class_ "form-control"
               , H.id_ "user"
               , H.name_ "user"
               , H.placeholder_ "user" ]
    H.div_ [ H.class_ "form-group" ] $ do
      H.label_ [ H.class_ "sr-only", H.for_ "password"] (toHtml ("password" :: Text))
      H.input_ [ H.type_ "password"
               , H.class_ "form-control"
               , H.id_ "password"
               , H.name_ "password"
               , H.placeholder_ "password" ]
    H.button_ [ H.type_ "submit"
              , H.classes_ ["btn", "btn-default"]
              ]
      (toHtml ("Login" :: Text))


data_ :: Text -> Text -> Attribute
data_ tag = attr_ (T.append "data-" tag)


aria_ :: Text -> Text -> Attribute
aria_ tag = attr_ (T.append "aria-" tag)


attr_ :: Text -> Text -> Attribute
attr_ = makeAttribute
