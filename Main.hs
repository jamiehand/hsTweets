{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import TwSearch

import Control.Applicative ((<$>), optional)
import Data.Monoid ((<>), mappend, mempty, mconcat)
-- TODO remove the monoid imports?
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
    H.p $ a ! href "/search?term=hello%20world"  $ "search"


-- query string parameters
-- queryParams :: ServerPart Response
-- queryParams =
--   do  mFoo <- optional $ lookText "foo"   -- lookText looks for query param
--       ok $ template "query params" $ do  -- passes title & body to template
--         p $ "foo is set to: " >> toHtml (show mFoo)
--         p $ "change the url to set it to something else."

-- search page
search :: ServerPart Response
search = msum [ viewForm, processForm ]
  where
    viewForm :: ServerPart Response
    viewForm =
      do  method GET
          mTerm <- optional $ lookText "term"
          ok $ template "form" $ do
            -- TODO put `searchForm` here instead of all this code
            form ! action "/search" ! enctype "multipart/form-data" ! A.method "POST" $ do
            -- TODO add as query param instead of form data
              label ! A.for "term"   $ "Search for a term"
              input ! type_ "text"   ! A.id "term" ! name "term"
              input ! type_ "submit" ! value "Search!"
            H.p $ "term is set to: " >> toHtml (show mTerm)
            case mTerm of
              Nothing   -> H.p $ "no results!" -- TODO really, we don't need anything to print here.
                              -- Ideally, we could show an empty page w/ search form in center.
              Just term -> do
                -- let (Just term) = mTerm -- NOTE: no need for this bc I can get term with case pattern matching
                H.p (toHtml ("Here are the most recent 10 results for " ++ unpack term ++ ":"))
                -- H.p (toHtml (head (searchContent 10 (unpack term)))) -- TODO remove "head"; how to  map over a list of Strings and make a paragraph (H.p) for each one?
                -- TODO can we <> H.p's together?
                foldl (\y x -> H.p (toHtml x) <> y) (H.p "Here are your results:") (searchContent 10 (unpack term))

    processForm :: ServerPart Response
    processForm =
      do  method POST
          term <- lookText "term"
          ok $ template "search" $ do
            -- TODO put `searchForm` here instead of all this code
            form ! action "/search" ! enctype "multipart/form-data" ! A.method "POST" $ do  -- TODO add query term to end of search URL here.
              label ! A.for "term"   $ "Search for a term"
              input ! type_ "text"   ! A.id "term" ! name "term"
              input ! type_ "submit" ! value "Search!"
            H.p (toHtml ("Here are the most recent 10 results for " ++ unpack term ++ ":"))
            -- H.p (toHtml (head (searchContent 10 (unpack term)))) -- TODO remove "head"; how to  map over a list of Strings and make a paragraph (H.p) for each one?
            -- TODO can we <> H.p's together?
            foldl (\y x -> H.p (toHtml x) <> y) (H.p "Here are your results:") (searchContent 10 (unpack term))
            -- TODO let user specify how many results they want? (e.g. between 1 and 100)
            -- TODO better to use foldr or foldl here? (or foldl' for lazy evaluation, if we're doing
            -- streaming...? NOTE: to use foldr, just switch the "y" and "x" at front of anon. fcn.)

-- TODO: the below, so that the code for viewForm and processForm don't repeat it.
-- searchForm :: Html -> Html


-- TODO it seems weird to me to have POST code that executes a search *and* GET code
-- that executes a search -- I feel like the POST should just execute GET
-- request with the correct query parameters, to allow us to have all of our
-- search execution in one place. But that would make for two requests instead
-- of one when a POST is made.... (i.e. the POST, and then the GET that the
-- POST calls.) So actually it is probably faster to h
-- It just means that, in terms of code complexity and maintenance, we'll have
-- to keep up code for search execution in *two* places OR figure out a way
-- to have the POST and GET both execute searches, but have them both call the
-- same external function that DRYs up our code and keeps search execution
-- functionality in one place!! :) :) :)
