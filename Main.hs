{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import TwSearch

import Control.Applicative ((<$>), optional)
import Data.Monoid ((<>), mappend, mempty, mconcat)
-- TODO remove the monoid imports?
import Data.Maybe (fromMaybe)
import Data.Text (Text, concat)
import qualified Data.Text as T

import Data.Text.Lazy (unpack)

import Network.HTTP (urlEncode)
import Happstack.Lite
import Text.Blaze.Internal (MarkupM)
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
  [ dir ""          $ homePage
  , dir "search"    $ search
  , dir "static"    $ serveStaticDir  -- serve static files
  , homePage  -- TODO make this a 404 page (w/ a link back home)?
  ]

template :: Text -> Html -> Response
template title body = toResponse $
  H.html $ do
    H.head $ do
      H.link ! A.rel "stylesheet" ! type_ "text/css" ! href "static/style.css"
      H.title (toHtml title)
    H.body $ do
      body
      p $ a ! href "/" $ "back home"


-- default handler
homePage :: ServerPart Response
homePage =
  ok $ template "home page" $ do
    H.h1 "Hello!"
    H.div ! A.class_ "gray" $ do
      H.p "Welcome to hsTweets!"
    H.p "Check out these killer apps."
    H.p $ a ! href "/search"  $ "search"

-- search page
search :: ServerPart Response
search = msum [ viewForm, processForm ]
  where
    viewForm :: ServerPart Response
    viewForm =
      do  method GET
          mTerm <- optional $ lookText "term"
          ok $ template "form" $ do
            case mTerm of
              Nothing   -> searchForm "empty-search-page"
              Just term -> do
                searchForm "results-search-page"
                displayResultsIfTerm (unpack term)


    processForm :: ServerPart Response
    processForm =
      do  method POST
          term <- lookText "term"
          let urlEncoded = urlEncode (unpack term)
          -- redirect to GET /search
          seeOther (("/search?term=" <> urlEncoded) :: String) (toResponse ())


searchForm :: H.AttributeValue -> Html
searchForm formClass =
  form ! A.class_ formClass ! action "/search"
  ! enctype "multipart/form-data" ! A.method "POST" $ do
    input ! type_ "text"   ! A.id "term" ! name "term" ! size "80"
    input ! type_ "submit" ! value "Search!"

displayResults :: [Char] -> MarkupM ()
displayResults term = do
  let results = searchContent 10 term
  case results of
    []        -> H.p (toHtml $ "There are no recent results for \"" ++ term ++ "\".")
    otherwise -> do
      H.p (toHtml $ "Here are the most recent 10 results for \"" ++ term ++ "\":")
      mconcat $ map (H.p . toHtml) results
      -- TODO let user specify how many results they want? (e.g. between 1 and 100)

displayResultsIfTerm :: [Char] -> MarkupM ()
displayResultsIfTerm term =
  case term of
    ""        -> "Please enter a search term."
    otherwise -> displayResults term

serveStaticDir :: ServerPart Response
-- There are no "index files" to the static directory, so we pass
-- serveDirectory an empty list as the 2nd arg. Not sure if this is good form.
serveStaticDir = serveDirectory DisableBrowsing [] "static"
