{-# LANGUAGE OverloadedStrings #-}

module Main where

import TwSearch

import Web.Spock
import Web.Spock.Config

import Control.Monad.Trans
import Data.Monoid
import Data.IORef
import qualified Data.Text as T

data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)

main :: IO ()
main =
    do ref <- newIORef 0
       spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
       runSpock 8080 (spock spockCfg app)

app :: SpockM () MySession MyAppState ()
app =
    do get root $
           text "Hello World!"
       get ("search" <//> var) $ \term ->
           do (DummyAppState ref) <- getState
              let results = foldr (\x y -> x <> "\n\n" <> y) "" (searchContent 10 (T.unpack term)) -- TODO get results from Twitter here
              text ("Here are your results for \"" <> term <> "\":\n\n" <> T.pack(results))
