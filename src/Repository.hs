{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Repository (createBeer, singleBeer, allBeers, deleteBeerById, updateBeer) where

import Data.Text (Text)
import Database.MongoDB ((!?), (=:))
import qualified Database.MongoDB as DB
import Model

collection :: Text
collection = "Beer"

runWithDB :: DB.Action IO a -> IO a
runWithDB act = do
  pipe <- DB.connect $ DB.host "localhost"
  let run = DB.access pipe DB.master "beersdb"
  run act

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

createBeer :: BeerWithId -> IO DB.ObjectId
createBeer beer =
  runWithDB $ do
    (DB.ObjId oid) <- DB.insert collection $ toDocument beer
    return oid

updateBeer :: BeerWithId -> IO ()
updateBeer beer =
  runWithDB $ DB.save collection $ toDocument beer

deleteBeerById :: DB.ObjectId -> IO ()
deleteBeerById beerId =
  runWithDB $
    DB.deleteOne (DB.select ["_id" =: beerId] collection)

allBeers :: IO [BeerWithId]
allBeers =
  runWithDB $ do
    docs <- DB.find (DB.select [] collection) >>= DB.rest
    return (fromDocument <$> docs)

singleBeer :: DB.ObjectId -> IO (Maybe BeerWithId)
singleBeer oid =
  runWithDB $ do
    doc <- DB.findOne (DB.select ["_id" =: oid] collection)
    return (fromDocument <$> doc)
