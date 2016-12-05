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

import Text.Blaze.Internal (MarkupM)


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

-- search page
search :: ServerPart Response
search = msum [ viewForm, processForm ]
  where
    viewForm :: ServerPart Response
    viewForm =
      do  method GET
          mTerm <- optional $ lookText "term"
          ok $ template "form" $ do
            searchForm
            H.p $ "term is set to: " >> toHtml (show mTerm)
            case mTerm of
              Nothing   -> H.p $ "no results!" -- TODO really, we don't need anything to print here.
                              -- Ideally, we could show an empty page w/ search form in center.
              Just term -> displayResultsIfTerm (unpack term)

    processForm :: ServerPart Response
    processForm =
      do  method POST
          term <- lookText "term"
          seeOther (("/search?term=" <> (unpack term)) :: String) (toResponse ())
          -- TODO keep the above redirect, or the below functionality (responding directly with the page to display)?
          -- ok $ template "search" $ do
          --   searchForm
          --   displayResultsIfTerm (unpack term)


-- TODO: the below, so that the code for viewForm and processForm don't repeat it.
searchForm :: Html
searchForm =
  form ! action "/search" ! enctype "multipart/form-data" ! A.method "POST" $ do
  -- TODO add term as query param to end of search URL here. (TODO, do this *instead of*, or *in addition to*, passing as form data?)
    label ! A.for "term"   $ "Search for a term"
    input ! type_ "text"   ! A.id "term" ! name "term"
    input ! type_ "submit" ! value "Search!"

displayResults :: [Char] -> Text.Blaze.Internal.MarkupM ()  -- TODO clean types up here?
displayResults term = do
  H.p (toHtml ("Here are the most recent 10 results for " ++ term ++ ":"))
  -- H.p (toHtml (head (searchContent 10 (unpack term)))) -- TODO remove this
  -- TODO understand this better: how we can <> H.p's together, but how is it working?
  foldl (\y x -> H.p (toHtml x) <> y) (H.p "Here are your results:") (searchContent 10 term)
  -- TODO let user specify how many results they want? (e.g. between 1 and 100)
  -- TODO better to use foldr or foldl here? (or foldl' for lazy evaluation, if we're doing
  -- streaming...? NOTE: to use foldr, just switch the "y" and "x" at front of anon. fcn.)
  -- TODO why do new results show up at top instead of bottom (e.g. why is "Here are your results" at the
  -- bottom of the page instead of at the top?)

displayResultsIfTerm :: [Char] -> MarkupM ()
displayResultsIfTerm term =
  case term of
    ""        -> "Please enter a search term."
    otherwise -> displayResults term

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
