{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import TwSearch

import Control.Applicative ((<$>), optional)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Happstack.Lite
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (action, enctype, href,
                                    name, size, type_, value)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A


main :: IO ()
main = serve (Just appConfig) app

appConfig = ServerConfig { port      = 8080
                         , ramQuota  = 1 * 10^6
                         , diskQuota = 20 * 10^6
                         , tmpDir    = "/tmp/"
                         }

app :: ServerPart Response
app = msum
  [ dir "search" $ search
  , homePage
  ]

template :: Text -> Html -> Response
template title body = toResponse $
  H.html $ do
    H.head $ do
      H.title (toHtml title)
    H.body $ do
      body
      p $ a ! href "/" $ "back home"

-- default handler
homePage :: ServerPart Response
homePage =
  ok $ template "home page" $ do
    H.h1 "Hello!"
    H.p "Welcome to hsTweets!"
    H.p "Check out these killer apps."
    H.p $ a ! href "/search?term=hello%20world"  $ "query parameters"

-- search page
search :: ServerPart Response
search = msum [ viewForm, processForm ]
  where
    viewForm :: ServerPart Response
    viewForm =
      do  method GET
          ok $ template "form" $
            form ! action "/search" ! enctype "multipart/form-data" !
            -- TODO add as query param instead of form data
            A.method "POST" $ do
              label ! A.for "term"   $ "Search for a term"
              input ! type_ "text"   ! A.id "term" ! name "term"
              input ! type_ "submit" ! value "Search!"
    processForm :: ServerPart Response
    processForm =
      do  method POST
          term <- lookText "term"
          ok $ template "search" $ do
            H.p (toHtml ("Here are the most recent 10 results for " ++ unpack term ++ ":"))
            H.p (toHtml (head (searchContent 10 (unpack term)))) -- TODO remove "head"; how to  map over a list of Strings and make a paragraph (H.p) for each one?
