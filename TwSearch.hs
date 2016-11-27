--searchContent takes an integer n and a query (string) and then produces a list of strings
--of the n most recent tweets containing the query

{-# LANGUAGE OverloadedStrings #-}

module TWSearch where

import qualified Data.Text as T
import Web.Twitter.Conduit
import Web.Twitter.Types.Lens
import Web.Twitter.Conduit.Parameters
import Control.Lens
import System.IO.Unsafe


tokens :: OAuth
tokens = twitterOAuth
    { oauthConsumerKey = "BxyMo5ixnQ6XQ3vFpJBhpJFhD"
    , oauthConsumerSecret = "6to67Z4XLiSGoN93K849tLFFScZlzQE8GkHCgiwQP8Kytl4xOY"
    }


credential :: Credential
credential = Credential
    [ ("oauth_token", "796924160786186240-S4PetT2w9YgmFCYVCXxBF2Jl0xoFdOt")
    , ("oauth_token_secret", "FWZSVbWM8LuReuGu8YDYsqQwbWuJ4oAdg2zZddhTN0SHG")
    ]

twInfo :: TWInfo
twInfo = def
    { twToken = def { twOAuth = tokens, twCredential = credential }
    , twProxy = Nothing
    }

getResult :: Integer -> String -> IO (SearchResult [Status])
getResult n str= do
    mgr <- newManager tlsManagerSettings
    call twInfo mgr $ (search $ T.pack str) & Web.Twitter.Conduit.Parameters.count ?~ (n)

getMaybe (Just a) = a

getURL :: Status -> T.Text
getURL x | ((getMaybe (x ^. statusEntities)) ^. enMedia) == [] = T.empty
         | otherwise                                           = (head ((getMaybe (x ^. statusEntities)) ^. enMedia)) ^. entityBody . meMediaURL


searchContent' :: Integer -> String -> [String]
searchContent' n str = Prelude.map T.unpack . Prelude.map f $ ((unsafePerformIO (getResult n str)) ^. searchResultStatuses)
                        where f x = T.concat [(x^. statusUser . userScreenName), ": ", (x^. statusText), " [Image URL:", (getURL x), "]" ]


searchContent :: Integer -> String -> (Maybe [String])
searchContent n str | (searchContent' n str) == [] = Nothing
                    | otherwise                    = Just (searchContent' n str)
