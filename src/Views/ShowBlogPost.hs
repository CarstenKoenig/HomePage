{-# LANGUAGE OverloadedStrings #-}
module Views.ShowBlogPost
  ( page
  ) where

import           Control.Monad (forM_)

import           Data.Default (def)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Time (TimeZone)
import qualified Data.Time as Time
import           Data.Time.LocalTime (TimeOfDay (..))
import qualified Data.Time.LocalTime as Time

import           Control.Monad.IO.Class (MonadIO)

import           Lucid (Html, toHtml, toHtmlRaw)
import qualified Lucid.Html5 as H
import qualified Lucid.Bootstrap as BS

import           Text.Blaze (preEscapedText, preEscapedToMarkup)
import qualified Text.Blaze.Html as TBH
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Markdown (Markdown(..), MarkdownSettings(..), markdown)
import           Text.Printf

import           Skylighting ( TokenizerConfig(..), SourceLine)
import qualified Skylighting as Sky       



import           Views.Page (Page (Page), PageContext (PageContext))
import qualified Views.Page as Page


import           Models.Blog


page :: BlogPost -> PageContext s -> Page
page post context =
  Page (Just cssStyles) (blogPostTitle post) $ pageContent (Page.timezone context) post


pageContent :: TimeZone -> BlogPost -> Html ()
pageContent timeZone post =
  H.div_ [ H.class_ "col-sm-8 blog-main" ] $ do
    H.p_ [ H.class_ "blog-post-meta" ] $ categoryLabels post
    H.h2_ [ H.class_ "blog-post-title" ] $ toHtml . blogPostTitle $ post
    H.p_ [ H.class_ "blog-post-meta" ] $ toHtml $ metaInfo timeZone post
    toHtmlRaw $ renderBlogPost post


metaInfo :: TimeZone -> BlogPost -> Text
metaInfo zone blogPost =
  case publishedAt of
    Nothing -> ""
    Just time -> publishedText time
  where
    publishedAt = Time.utcToLocalTime zone <$> blogPostPublishedTime blogPost
    publishedText time =
      let (TimeOfDay h m _)  = Time.localTimeOfDay time
          (y, mn, d)         = Time.toGregorian $ Time.localDay time
      in T.pack $ printf "published  %02d/%02d/%d at %02d:%02d" mn d y h m



categoryLabels :: BlogPost -> Html ()
categoryLabels blogPost = forM_ (blogPostCategories blogPost) label
  where
    label :: Category -> Html ()
    label (Category name) =
      H.span_ [ H.class_ "label label-info" ] (toHtml name)


renderBlogPost :: BlogPost -> Text
renderBlogPost blogPost =
  TL.toStrict $ renderHtml $ markdown def
     { msBlockCodeRenderer = renderer
     , msXssProtect        = False
     } md
  where
    (Markdown md) = blogPostContent blogPost
    renderer (Just "math") (src,_) = renderMath src
    renderer (Just "iframe") (src,_) = renderFrame src
    renderer lang (src,_) = renderLang lang src


renderLang :: Maybe Text -> Text -> TBH.Html
renderLang lang src =
  Sky.formatHtmlBlock Sky.defaultFormatOpts
  $ highlightAs (fromMaybe "haskell" lang) src


renderFrame :: Text -> TBH.Html
renderFrame src =
  preEscapedText $
  T.concat [ "<iframe width=\"560\" height=\"315\" "
           , "src=\""
           , src
           , "\" frameborder=\"0\" allowfullscreen></iframe>" ]


renderMath :: Text -> TBH.Html
renderMath src =
  preEscapedToMarkup $
  T.concat ["$$\n", src, "\n$$"]


cssStyles :: Text
cssStyles = T.pack $ Sky.styleToCss Sky.pygments


highlightAs :: Text -> Text -> [SourceLine]
highlightAs lang source =
  case Sky.syntaxByName Sky.defaultSyntaxMap lang of
    Nothing -> []
    Just syntax ->
      case Sky.tokenize config syntax source of
        Left _ -> []
        Right lines -> lines
  where
    config = TokenizerConfig Sky.defaultSyntaxMap False
