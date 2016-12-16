{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module OtherHandlers where

import HtmlTemplate

import Data.Text (Text)

import Happstack.Lite
import Text.Blaze.Html5 (Html, (!), a, p, toHtml)
import Text.Blaze.Html5.Attributes (href, type_)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A


homePage :: ServerPart Response
homePage =
  ok $ template "home page" $ do
    H.div ! A.class_ "blocktext" $ do
      H.h1 "Welcome to hsTweets!"
      H.p "To search for a term on Twitter, check out:"
      H.p $ a ! href "/search"  $ "search"
      H.p "For more on our process and this project, check out:"
      H.p $ a ! href "/about"   $ "about"


about :: ServerPart Response
about =
  ok $ template "about" $ do
    H.div ! A.class_ "blocktext" $ do
      H.h1 "About this project"
      H.p "hsTweets was created by Brenda Li and Jamie Hand as our final project \
      \for Functional Programming (Fall 2016, Middlebury College, with Professor Pete \
      \Johnson). We used Haskell (plus HTML and CSS), along with some Haskell \
      \packages to create a server that allows the user to search for key terms \
      \on Twitter."
      H.p "We used Happstack Lite for our server backend (which includes Blaze \
      \for HTML rendering). For accessing Twitter's API, we used Twitter-conduit."
      H.p $ a ! href "https://github.com/jamiehand/hsTweets" $ "Check out our code on GitHub!"
      H.p $ a ! href "http://logomakr.com" $ "Our logos were made with Logo Maker."

serveStaticDir :: ServerPart Response
-- There are no "index files" to the static directory, so we pass
-- serveDirectory an empty list as the 2nd arg. Not sure if this is good form.
serveStaticDir = serveDirectory DisableBrowsing [] "static"

notFoundHandler :: ServerPart Response
notFoundHandler =
  ok $ template "not found" $ do
    H.div ! A.class_ "blocktext" $ do
      H.h1 "Not found"
      H.p "The page you are looking for does not exist!"
