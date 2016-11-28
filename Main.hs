{-# LANGUAGE OverloadedStrings #-}

module Main where

import TwSearch

import Web.Spock
import Web.Spock.Config

import Control.Monad.Trans
import Data.Monoid
import Data.IORef
import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H

import qualified Text.Digestive.Bootstrap as B
import qualified Text.Digestive.Form      as F
import qualified Text.Digestive.Types     as Ty

-- TODO remove whichever of the below is not needed for "runForm"
import Text.Digestive
-- import Text.Digestive.Blaze.Html5
import Text.Digestive.Happstack
import Text.Digestive.Util

data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)


data SearchRequest
    = SearchRequest
    { queryTerm     :: T.Text
    } deriving (Show)

searchFormSpec :: B.FormMeta
searchFormSpec =
    B.FormMeta
    { B.fm_method = POST
    , B.fm_target = "/search"
    , B.fm_elements =
        [ B.FormElement "term" (Just "Enter a term") B.InputText
        ]
    , B.fm_submitText = "Search"
    }

searchForm :: Monad m => F.Form H.Html m SearchRequest
searchForm =
    SearchRequest
    <$> "queryTerm"     F..: F.validate (minMaxLen (1, 120)) (F.text Nothing)  -- initial default value
    -- <*> "password" F..: F.validate (minMaxLen (6, 40)) (F.text Nothing)

minMaxLen :: (Int, Int) -> T.Text -> Ty.Result H.Html T.Text
minMaxLen (minLen, maxLen) t =
    if len >= minLen && len <= maxLen
        then Ty.Success stripped
        else Ty.Error $ H.toHtml $
            "Must be longer than "  ++ show minLen
            ++ " and shorter than " ++ show maxLen ++ " characters"
    where
        stripped = T.strip t
        len = T.length stripped

searchAction :: SpockAction conn MySession st ()
searchAction =
    do  let formView = B.renderForm searchFormSpec

        f <- runForm "searchForm" searchForm
        case f of
            -- (view, Nothing) ->
            --     formView view  -- site combinator: run full site, not just a little
            --     -- site $ formView view  -- site combinator: run full site, not just a little
                                      -- chunk (https://youtu.be/kNqsOBrCbLo?t=21m40s)
            (view, Just searchReq) ->
                do redirect ((T.pack "/search/hello")) -- TODO  ++ (queryTerm searchReq))

-- main =
--     runSpock 3000 $ spock cfg $
--     -- do get     "/" $ redirect "/login"
--        getpost "login" searchAction  -- wires searchAction to a route ^_^
--        get "logout" $ writeSession Nothing >> redirect "/"  -- logout (session has NO user)


main :: IO ()
main =
    do ref <- newIORef 0
       spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
       runSpock 8080 (spock spockCfg app)


app :: SpockM () MySession MyAppState ()
app =
    do get root $
            -- TODO get these back to just "text"
           Web.Spock.text "Hello World!"
       -- TODO separate out the code below into its own function; then let
       -- "get/term" and "post" w/ a term in data evaluate to the separated-out code
       getpost "search" searchAction
       get ("search" <//> var) $ \term ->
           do (DummyAppState ref) <- getState
              let results = foldr (\x y -> x <> "\n\n" <> y) "" (searchContent 10 (T.unpack term)) -- TODO get results from Twitter here
              Web.Spock.text ("Here are your results for \"" <> term <> "\":\n\n" <> T.pack(results))
