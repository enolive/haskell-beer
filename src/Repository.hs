{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Repository (createBeer, singleBeer, allBeers, deleteBeerById, updateBeer) where

import Model
import qualified Database.MongoDB as DB
import Database.MongoDB ((!?), (=:))

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
