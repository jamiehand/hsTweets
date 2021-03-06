{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import qualified SearchHandler as Search
import qualified OtherHandlers as Handlers

import Happstack.Lite

main :: IO ()
main = serve (Just appConfig) app

appConfig = ServerConfig { port      = 8080
                         , ramQuota  = 1 * 10^6
                         , diskQuota = 20 * 10^6
                         , tmpDir    = "/tmp/"
                         }

app :: ServerPart Response
app = msum
  [ dir "index"     $ Handlers.homePage
  , dir "about"     $ Handlers.about
  , dir "search"    $ Search.search
  , dir "static"    $ Handlers.serveStaticDir  -- serve static files
  , Handlers.notFoundHandler  -- TODO make this a 404 page (w/ a link back home)?
  ]
