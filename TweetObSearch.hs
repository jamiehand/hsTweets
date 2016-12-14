--tweetList takes an int n and a string and outputs a list of Tweet objects of
--the n most recent tweets containing the search string
{-# LANGUAGE OverloadedStrings #-}

module TweetObSearch where

import qualified Data.Text as T
import Web.Twitter.Conduit
import Web.Twitter.Types.Lens
import Web.Twitter.Conduit.Parameters
import Control.Lens
import System.IO.Unsafe
import Data.Default
import Data.HashMap.Strict
import Data.Time

data Tweet = Tweet { text :: String
                   , username :: String
                   , followers :: Int
                   , language :: String
                   , snapshotLink :: String
                   , tweetLink :: String
                   , snapshotWidth :: Maybe Int
                   , snapshotHeight :: Maybe Int
                   , id :: Integer
                   , time :: UTCTime }
    deriving Show

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

getResult :: Int -> String -> IO (SearchResult [Status])
getResult n str= do
    mgr <- newManager tlsManagerSettings
    call twInfo mgr $ (search $ T.pack str) & Web.Twitter.Conduit.Parameters.count ?~ (toInteger n)

extractStatus :: Int -> String -> [Status]
extractStatus n str = ((unsafePerformIO (getResult n str)) ^. searchResultStatuses)

getMaybe (Just a) = a

getURL :: Status -> String
getURL x | ((getMaybe (x ^. statusEntities)) ^. enMedia) == [] = ""
         | otherwise                                           = T.unpack y
            where y = (head ((getMaybe (x ^. statusEntities)) ^. enMedia)) ^. entityBody . meMediaURL

getFollow :: Status -> Int
getFollow x = x^. statusUser . userFollowersCount

getText :: Status -> String
getText x = T.unpack y
              where y = T.replace (T.pack "&amp;") (T.pack "&") (x^. statusText)

getUsername :: Status -> String
getUsername x = T.unpack y
                  where y = x^.statusUser . userScreenName

getLanguage :: Status -> String
getLanguage x = getMaybe (x^.statusLang)

getID :: Status -> Integer
getID x = x^.statusId

getWidth :: Status -> (Maybe Int)
getWidth x | ((getMaybe (x ^. statusEntities)) ^. enMedia) == [] = Nothing
           | otherwise                                           = (Just ((snd (head (toList y))) ^. msWidth))
                where y = ((head (getMaybe (x^.statusEntities) ^. enMedia)) ^.entityBody) ^. meSizes

getHeight :: Status -> (Maybe Int)
getHeight x | ((getMaybe (x ^. statusEntities)) ^. enMedia) == [] = Nothing
            | otherwise                                           = (Just ((snd (head (toList y))) ^. msHeight))
                where y = ((head (getMaybe (x^.statusEntities) ^. enMedia)) ^.entityBody) ^. meSizes

getTweetLink :: Status -> String
getTweetLink x = "https://twitter.com/statuses/" ++ (show (getID x))

getTime :: Status -> UTCTime
getTime x = x ^. statusCreatedAt

buildTweet :: Status -> Tweet
buildTweet x = Tweet (getText x)
                     (getUsername x)
                     (getFollow x)
                     (getLanguage x)
                     (getURL x)
                     (getTweetLink x)
                     (getWidth x)
                     (getHeight x)
                     (getID x)
                     (getTime x)

tweetList :: Int -> String -> [Tweet]
tweetList n str = Prelude.map buildTweet . extractStatus n $ str
