{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module SearchHandler where

import TweetObSearch
import HtmlTemplate

import Control.Applicative ((<$>), optional)
import Data.Monoid ((<>), mappend, mempty, mconcat)
import Data.Maybe (fromMaybe)
import Data.Text (Text, concat)
import qualified Data.Text as T
import Data.String as S

import Data.Text.Lazy (unpack)

import Network.HTTP (urlEncode)
import Happstack.Lite
import Text.Blaze.Internal (MarkupM)
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (action, enctype, href,
                                    name, size, type_, value)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A


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
  let statuses = tweetList 10 term
  let results = map (\x -> " : " ++ x) (map text statuses)
  let photos = map snapshotLink statuses
  let links = map tweetLink statuses
  let names = map username statuses
  case results of
    []        -> H.p (toHtml $ "There are no recent results for \"" ++ term ++ "\".")
    otherwise -> do
      let usernames = map linkAndUser (zip links names)
      let imgs = map (thumbSize) photos
      let ps   = map (toHtml) results
      let text = map (combineLinkText) (zip usernames ps)
      H.p (toHtml $ "Here are the 10 most recent results for \"" ++ term ++ "\":")
      H.p (mconcat $ tuplesToList (zip text imgs))

linkAndUser :: (String, String) -> Html
linkAndUser (link,name) = a ! href (S.fromString link) $ (S.fromString name)

combineLinkText :: (Html,Html) -> Html
combineLinkText (link,text) = H.p (mconcat (link:text: []))

tuplesToList :: [(a,a)] -> [a]
tuplesToList [] = []
tuplesToList ((x,y):xs) = x:y:tuplesToList xs

thumbSize :: String -> Html
thumbSize photo | photo == "" = H.img ! (A.src (S.fromString photo)) ! A.height "0" ! A.class_ "invisible"
                | otherwise   = H.img ! (A.src (S.fromString photo)) ! A.height "200"

displayResultsIfTerm :: [Char] -> MarkupM ()
displayResultsIfTerm term =
  case term of
    ""        -> "Please enter a search term."
    otherwise -> displayResults term
