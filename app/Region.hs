{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Region where

import Data.Aeson
import Data.Map 
import GHC.Generics

newtype RegionId = RegionId String deriving (Show, Eq, Ord, FromJSON, ToJSON, ToJSONKey, FromJSONKey)
newtype Borders = Borders (Map RegionId [RegionId]) deriving (FromJSON, ToJSON)
newtype FactionId = FactionId Integer deriving (Show, Eq, Ord, FromJSON, ToJSON)

data RegionInfo = RegionInfo {faction :: FactionId, population :: Integer} deriving(Generic, Show)
newtype GameMap = GameMap { gameMapToMap :: (Map RegionId RegionInfo) } deriving (FromJSON, ToJSON) 

instance FromJSON RegionInfo
instance ToJSON RegionInfo

baseRegions :: Int -> Int -> GameMap
baseRegions x y = GameMap $ fromList $ fmap (\id -> (id, RegionInfo (getFaction id) 2)) $ allRegionRegionId mkRegionId x y

getFaction :: RegionId -> FactionId
getFaction (RegionId "2_2") = FactionId 1
getFaction (RegionId "6_8") = FactionId 2
getFaction _     = FactionId 0

allRegionRegionId f x y = do
    x' <- [0..x]
    fmap (f x') [0..y]


mkRegionId :: Int -> Int -> RegionId
mkRegionId x y = RegionId $ show x ++ "_" ++ show y

neighboor maxX maxY (x, y) = (mkRegionId x y , allNeighboor)
    where
        allNeighboor = do
            x' <- [(max 0 $ x-1)..(min maxX $ x+1)]
            y' <- [(max 0 $ y-1)..(min maxY $ y+1)]
            return $ mkRegionId x' y'
 


borders :: Int -> Int -> Borders
borders x y = Borders $ fromList $ fmap (neighboor x y) $ allRegionRegionId (,) x y