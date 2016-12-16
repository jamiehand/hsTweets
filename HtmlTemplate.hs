{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module HtmlTemplate where

import Data.Text (Text)

import Happstack.Lite
import Text.Blaze.Html5 (Html, (!), a, p, toHtml)
import Text.Blaze.Html5.Attributes (href, type_)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

template :: Text -> Html -> Response
template title body = toResponse $
  H.html $ do
    H.head $ do
      H.link ! A.rel "stylesheet" ! type_ "text/css" ! href "static/style.css"
      H.title (toHtml title)
    H.body $ do
      a ! href "/index" $ H.img ! A.src "/static/logoWithText.png" ! A.class_ "logo" ! A.height "100"
      body
      a ! href "/index" $ H.img ! A.src "/static/logo.png"  ! A.class_ "logo" ! A.height "50"
