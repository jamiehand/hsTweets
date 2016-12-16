{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module OtherHandlers where

import HtmlTemplate

import Data.Text (Text)

import Happstack.Lite
import Text.Blaze.Html5 (Html, (!), a, p, toHtml)
import Text.Blaze.Html5.Attributes (href, type_)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

-- default handler
homePage :: ServerPart Response
homePage =
  ok $ template "home page" $ do
    H.div ! A.class_ "blocktext" $ do
      H.h1 "Hello!"
      H.div ! A.class_ "gray" $ do
        H.p "Welcome to hsTweets!"
      H.p $ a ! href "/search"  $ "search"
      H.p $ a ! href "/about"   $ "about"


about :: ServerPart Response
about =
  ok $ template "about" $ do
    H.h1 "About this project"
    H.p $ a ! href "http://logomakr.com" $ "Our logos were made with Logo Maker."

serveStaticDir :: ServerPart Response
-- There are no "index files" to the static directory, so we pass
-- serveDirectory an empty list as the 2nd arg. Not sure if this is good form.
serveStaticDir = serveDirectory DisableBrowsing [] "static"

notFoundHandler :: ServerPart Response
notFoundHandler =
  notFound $ template "not found" $ do
    H.h1 "Not found"
    H.p "The page you are looking for does not exist!"
