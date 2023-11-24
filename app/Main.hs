{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (mzero)
import Data.Aeson.Types
import Data.Text
import Data.Text.Lazy (fromStrict)
import Database.MongoDB ((!?), (=:))
import qualified Database.MongoDB as DB
import GHC.Generics (Generic)
import Network.HTTP.Types
import Web.Scotty

data BeerWithId = BeerWithId
  { _id :: Maybe DB.ObjectId,
    value :: Beer
  }
  deriving (Show, Generic)

data Beer = Beer
  { brand :: Text,
    name :: Text,
    strength :: Double
  }
  deriving (Show)

instance ToJSON BeerWithId where
  toJSON BeerWithId {value = Beer {..}, ..} =
    object
      [ "_id" .= (show <$> _id),
        "brand" .= brand,
        "name" .= name,
        "strength" .= strength
      ]

instance FromJSON Beer where
  parseJSON (Object v) =
    Beer <$> v .: "brand" <*> v .: "name" <*> v .: "strength"
  parseJSON _ = mzero

instance Parsable DB.ObjectId where
  parseParam = readEither

withEmptyId :: Beer -> BeerWithId
withEmptyId value =
  BeerWithId {_id = Nothing, value = value}

withId :: DB.ObjectId -> Beer -> BeerWithId
withId oid beer = BeerWithId {_id = Just oid, value = beer}

toDocument :: BeerWithId -> DB.Document
toDocument BeerWithId {value = Beer {..}, ..} =
  [ "brand" =: brand,
    "name" =: name,
    "strength" =: strength
  ]
    ++ beerId
  where
    beerId = case _id of
      Nothing -> []
      Just x -> ["_id" =: x]

fromDocument :: DB.Document -> BeerWithId
fromDocument doc =
  BeerWithId
    { _id = doc !? "_id",
      value =
        Beer
          { brand = DB.at "brand" doc,
            strength = DB.at "strength" doc,
            name = DB.at "name" doc
          }
    }

main :: IO ()
main = do
  pipe <- DB.connect $ DB.host "localhost"
  let run act = DB.access pipe DB.master "beers" act
  scotty 3000 $ do
    get "/beers" $ do
      beers <- liftAndCatchIO $ run allBeers
      json beers
    get "/beers/:id" $ do
      oid <- objectIdFromPath
      found <- liftAndCatchIO $ run $ singleBeer oid
      case found of
        Nothing -> status noContent204
        Just beer -> json beer
    post "/beers" $ do
      beer <- beerFromRequestBody
      oid <- liftAndCatchIO $ run $ createBeer $ withEmptyId beer
      status created201
      addHeader "Location" ("/beers/" <> (fromStrict . pack . show) oid)
      json $ BeerWithId {_id = Just oid, value = beer}
    put "/beers/:id" $ do
      oid <- objectIdFromPath
      existing <- liftAndCatchIO $ run $ singleBeer oid
      case existing of
        Nothing -> status noContent204
        Just _ -> do
          updated <- withId oid <$> beerFromRequestBody
          liftAndCatchIO $ run $ updateBeer updated
          json updated
    delete "/beers/:id" $ do
      oid <- objectIdFromPath
      liftAndCatchIO $ run $ deleteBeerById oid
      status noContent204
  DB.close pipe

objectIdFromPath :: ActionM DB.ObjectId
objectIdFromPath = param "id"

beerFromRequestBody :: ActionM Beer
beerFromRequestBody = jsonData

createBeer :: BeerWithId -> DB.Action IO DB.ObjectId
createBeer beer = do
  (DB.ObjId oid) <- DB.insert "beer" $ toDocument beer
  return oid

updateBeer :: BeerWithId -> DB.Action IO ()
updateBeer beer =
  DB.save "beer" $ toDocument beer

deleteBeerById :: DB.ObjectId -> DB.Action IO ()
deleteBeerById beerId =
  DB.deleteOne (DB.select ["_id" =: beerId] "beer")

allBeers :: DB.Action IO [BeerWithId]
allBeers = do
  beers <- DB.find (DB.select [] "beer") >>= DB.rest
  return (fromDocument <$> beers)

singleBeer :: DB.ObjectId -> DB.Action IO (Maybe BeerWithId)
singleBeer oid = do
  doc <- DB.findOne (DB.select ["_id" =: oid] "beer")
  return (fromDocument <$> doc)
