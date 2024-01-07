{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Api (readAllBeersA, readSingleBeerA, createBeerA, updateBeerA, deleteBeerA) where

import Data.Text
import Data.Text.Lazy (fromStrict)
import qualified Database.MongoDB as DB
import Model
import Network.HTTP.Types.Status
import qualified Repository
import Web.Scotty

readAllBeersA :: ActionM ()
readAllBeersA = do
  beers <- liftAndCatchIO Repository.allBeers
  json beers

readSingleBeerA :: ActionM ()
readSingleBeerA = do
  oid <- objectIdFromPath
  found <- liftAndCatchIO $ Repository.singleBeer oid
  case found of
    Nothing -> status noContent204
    Just beer -> json beer

createBeerA :: ActionM ()
createBeerA = do
  beer <- beerFromRequestBody
  oid <- liftAndCatchIO $ Repository.createBeer $ withEmptyId beer
  status created201
  let hexId = (fromStrict . pack . show) oid
  addHeader "Location" ("/beers/" <> hexId)
  json $ BeerWithId {_id = Just oid, value = beer}

updateBeerA :: ActionM ()
updateBeerA = do
  oid <- objectIdFromPath
  existing <- liftAndCatchIO $ Repository.singleBeer oid
  case existing of
    Nothing -> status noContent204
    Just _ -> do
      updated <- withId oid <$> beerFromRequestBody
      liftAndCatchIO $ Repository.updateBeer updated
      json updated

deleteBeerA :: ActionM ()
deleteBeerA = do
  oid <- objectIdFromPath
  liftAndCatchIO $ Repository.deleteBeerById oid
  status noContent204

objectIdFromPath :: ActionM DB.ObjectId
objectIdFromPath = param "id"

beerFromRequestBody :: ActionM Beer
beerFromRequestBody = jsonData

-- make ObjectId readable from path
instance Parsable DB.ObjectId where
  parseParam = readEither
