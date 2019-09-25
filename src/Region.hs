{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Region where

import PlayerManagement
import Data.Aeson
import Data.Map 
import GHC.Generics
import Control.Lens
import Data.List
import Data.Ord


newtype RegionId = RegionId (Int, Int) deriving (Show, Eq, Ord, FromJSON, ToJSON, ToJSONKey, FromJSONKey)

newtype Borders = Borders (Map RegionId [RegionId]) deriving (FromJSON, ToJSON)
newtype FactionId = FactionId Integer deriving (Show, Eq, Ord, FromJSON, ToJSON)
newtype Army = Army Int deriving (Show, Eq, Ord, FromJSON, ToJSON, Num)
data RegionInfo = RegionInfo {_faction :: Maybe PlayerId, _population :: Army} deriving(Show, Generic, Eq)

instance FromJSON RegionInfo
instance ToJSON RegionInfo


makeLenses ''RegionInfo

type GameMap = Map RegionId RegionInfo


baseRegions :: Int -> Int -> GameMap
baseRegions x y = fromList $ fmap (\id -> (id, RegionInfo (getFaction id) 2)) $ allRegionRegionId mkRegionId x y

getFaction :: RegionId -> Maybe PlayerId
getFaction (RegionId (2,2)) = Just $ PlayerId 1
getFaction (RegionId (6,8)) = Just $ PlayerId 2
getFaction _     = Nothing

allRegionRegionId f x y = do
    x' <- [0..x]
    fmap (f x') [0..y]


mkRegionId :: Int -> Int -> RegionId
mkRegionId x y = RegionId $ (x,y)

neighboor maxX maxY (x, y) = (mkRegionId x y , allNeighboor)
    where
        allNeighboor = do
            x' <- [(max 0 $ x-1)..(min maxX $ x+1)]
            y' <- [(max 0 $ y-1)..(min maxY $ y+1)]
            return $ mkRegionId x' y'
 

distance :: RegionId -> RegionId -> Int
distance (RegionId (x1, y1)) (RegionId (x2,y2)) = abs (x2 - x1) + abs (y2 - y1) 


borders :: Int -> Int -> Borders
borders x y = Borders $ fromList $ fmap (neighboor x y) $ allRegionRegionId (,) x y

findClosest :: RegionId -> [RegionId] -> RegionId
findClosest origin = minimumBy (comparing (distance origin))