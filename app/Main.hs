{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api
import Network.Wai.Middleware.RequestLogger
import Web.Scotty

main :: IO ()
main = do
  scotty 3010 $ do
    middleware logStdoutDev
    get "/beers" readAllBeersA
    get "/beers/:id" readSingleBeerA
    post "/beers" createBeerA
    put "/beers/:id" updateBeerA
    delete "/beers/:id" deleteBeerA
