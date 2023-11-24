{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson.Types
import Data.Text
import qualified Database.MongoDB as DB
import GHC.Generics (Generic)
import Web.Scotty

newtype Response = Response {content :: Text}
  deriving (Generic, ToJSON)

data Beer = Beer
  { brand :: String,
    name :: String,
    strength :: Float
  }

main :: IO ()
main = do
  pipe <- DB.connect $ DB.host "localhost"
  e <- DB.access pipe DB.master "beers" run
  DB.close pipe
  print e

run :: DB.Action IO [DB.Document]
run = do
  createBeers
  allBeers

createBeers :: DB.Action IO DB.Value
createBeers = do
  let beer = Beer {brand = "Schanze", name = "Rot", strength = 5.0}
  DB.insert
    "beer"
    ["brand" DB.=: ("Schanze" :: String), "name" DB.=: ("Rot" :: String), "strength" DB.=: (5.0 :: Float)]

allBeers :: DB.Action IO [DB.Document]
allBeers = do
  beers <- DB.find (DB.select [] "beer")
  DB.rest beers

startupServer :: IO ()
startupServer = scotty 3000 $
  get "/scotty/:word" $ do
    beam <- param "word"
    json $ Response {content = "Scotty, " <> beam <> " me up!"}
