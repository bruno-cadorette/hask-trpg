{-# LANGUAGE DeriveGeneric #-}
module Region where

import Data.Aeson
import Data.Map 
import GHC.Generics

type RegionId = String
newtype FactionId = FactionId Integer deriving (Generic, Show, Eq, Ord)

instance FromJSON FactionId
instance ToJSON FactionId


data RegionInfo = RegionInfo {faction :: FactionId, population :: Integer} deriving(Generic, Show)
type GameMap = Map RegionId RegionInfo

instance FromJSON RegionInfo
instance ToJSON RegionInfo

baseRegions :: Int -> Int -> GameMap
baseRegions x y = fromList $ fmap (\id -> (id, RegionInfo (getFaction id) 2)) $ allRegionRegionId mkRegionId x y


getFaction "2_2" = FactionId 1
getFaction "6_8" = FactionId 2
getFaction _     = FactionId 0

allRegionRegionId f x y = do
    x' <- [0..x]
    fmap (f x') [0..y]


mkRegionId x y = show x ++ "_" ++ show y

neighboor maxX maxY (x, y) = (mkRegionId x y , allNeighboor)
    where
        allNeighboor = do
            x' <- [(max 0 $ x-1)..(min maxX $ x+1)]
            y' <- [(max 0 $ y-1)..(min maxY $ y+1)]
            return $ mkRegionId x' y'
 


borders :: Int -> Int -> Map RegionId [RegionId]
borders x y = fromList $ fmap (neighboor x y) $ allRegionRegionId (,) x y