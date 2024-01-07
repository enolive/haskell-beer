{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Data.Text
import Data.Text.Lazy (fromStrict)
import qualified Database.MongoDB as DB
import Model
import Network.HTTP.Types
import Network.Wai.Middleware.RequestLogger
import qualified Repository
import Web.Scotty
import Control.Monad.IO.Class (MonadIO)

instance Parsable DB.ObjectId where
  parseParam = readEither

main :: IO ()
main = do
  pipe <- DB.connect $ DB.host "localhost"
  let run = DB.access pipe DB.master "beersdb" :: MonadIO m => DB.Action m a -> m a
  scotty 3010 $ do
    middleware logStdoutDev
    get "/beers" $ do
      beers <- liftAndCatchIO $ run Repository.allBeers
      json beers
    get "/beers/:id" $ do
      oid <- objectIdFromPath
      found <- liftAndCatchIO $ run $ Repository.singleBeer oid
      case found of
        Nothing -> status noContent204
        Just beer -> json beer
    post "/beers" $ do
      beer <- beerFromRequestBody
      oid <- liftAndCatchIO $ run $ Repository.createBeer $ withEmptyId beer
      status created201
      addHeader "Location" ("/beers/" <> (fromStrict . pack . show) oid)
      json $ BeerWithId {_id = Just oid, value = beer}
    put "/beers/:id" $ do
      oid <- objectIdFromPath
      existing <- liftAndCatchIO $ run $ Repository.singleBeer oid
      case existing of
        Nothing -> status noContent204
        Just _ -> do
          updated <- withId oid <$> beerFromRequestBody
          liftAndCatchIO $ run $ Repository.updateBeer updated
          json updated
    delete "/beers/:id" $ do
      oid <- objectIdFromPath
      liftAndCatchIO $ run $ Repository.deleteBeerById oid
      status noContent204
  DB.close pipe

objectIdFromPath :: ActionM DB.ObjectId
objectIdFromPath = param "id"

beerFromRequestBody :: ActionM Beer
beerFromRequestBody = jsonData
