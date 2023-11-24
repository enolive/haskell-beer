{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Model where

import Database.MongoDB as DB
import GHC.Generics
import Data.Text
import Data.Aeson
import Control.Monad (mzero)
  
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

withEmptyId :: Beer -> BeerWithId
withEmptyId value =
  BeerWithId {_id = Nothing, value = value}

withId :: DB.ObjectId -> Beer -> BeerWithId
withId oid beer = BeerWithId {_id = Just oid, value = beer}
