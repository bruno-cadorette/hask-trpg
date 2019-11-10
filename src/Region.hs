{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, BlockArguments, TypeApplications, TypeFamilies #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds, RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
module Region where

import PlayerManagement
import Data.Aeson
import Data.Aeson.Types
import Data.Map 
import GHC.Generics
import Control.Lens
import Data.List
import Data.Text
import Data.Text.Read
import Data.Ord
import Polysemy
import Polysemy.State
import Soldier

newtype RegionId = RegionId (Int, Int) deriving (Show, Eq, Ord, FromJSONKey)

instance ToJSON RegionId where
    toJSON = String . encodeRegionId
instance FromJSON RegionId where
    parseJSON = withText "RegionId" parseRegionId
            
instance ToJSONKey RegionId where
    toJSONKey = toJSONKeyText encodeRegionId


encodeRegionId (RegionId (x, y)) = pack $ show x <> "_" <> show y

parseRegionId id =
    let (x, y) = breakOn "_" id in
    case (\x y ->  RegionId (fst x, fst y)) <$> decimal x <*> decimal (Data.Text.tail y) of
        Right regionId -> pure regionId
        Left error -> fail error

newtype Borders = Borders (Map RegionId [RegionId]) deriving (FromJSON, ToJSON)
newtype Army = Army Int deriving (Show, Eq, Ord, FromJSON, ToJSON, Num)
class Region a where
    regionId :: a -> RegionId

instance Region RegionId where
    regionId = id

instance Region EmptyRegion where
    regionId (EmptyRegion regionId) = regionId

instance Region TargetSoldier where
    regionId (TargetSoldier regionId _) = regionId
    

instance Soldier TargetSoldier where
    soldier (TargetSoldier _ soldier) = soldier

newtype EmptyRegion = EmptyRegion RegionId deriving(Show)
data TargetSoldier = TargetSoldier RegionId SoldierUnit deriving (Show)

makeLenses ''TargetSoldier 

data UnitAction m a where
    LoseHP :: Int -> TargetSoldier -> UnitAction m () 
    Move :: TargetSoldier -> EmptyRegion -> UnitAction m ()

makeSem ''UnitAction

data ReadMapInfo m a where
    GetUnit :: RegionId -> ReadMapInfo m (Maybe SoldierUnit)

makeSem ''ReadMapInfo

getTargetSoldier :: Member ReadMapInfo r => RegionId -> Sem r (Maybe TargetSoldier)
getTargetSoldier regionId = 
    fmap (fmap (TargetSoldier regionId)) $ getUnit regionId

type UnitPositions = Map RegionId SoldierUnit

runUnitMoving ::Members '[State UnitPositions] r => InterpreterFor UnitAction r
runUnitMoving = interpret $ \case
    Move (TargetSoldier _ playerToMove) (EmptyRegion destination) -> 
        modify (Data.Map.insert destination playerToMove)
    LoseHP damage (TargetSoldier location _) -> 
        modify (Data.Map.update (hitSoldier damage) location)


runReadMapInfo :: Members '[State UnitPositions] r => InterpreterFor ReadMapInfo r
runReadMapInfo = interpret $ \(GetUnit regionId) -> gets (Data.Map.lookup regionId)


baseUnitPositions :: UnitPositions
baseUnitPositions = fromList [(RegionId (2,2), baseSoldier (PlayerId 1)), (RegionId (6,8), baseSoldier (PlayerId 2)) ]

allRegionRegionId f x y = do
    x' <- [0..x]
    fmap (f x') [0..y]


mkRegionId :: Int -> Int -> RegionId
mkRegionId x y = RegionId (x,y)

neighboor maxX maxY (x, y) = (mkRegionId x y , allNeighboor)
    where
        allNeighboor = do
            x' <- [(max 0 $ x-1)..(min maxX $ x+1)]
            fmap (mkRegionId x') [(max 0 $ y-1)..(min maxY $ y+1)]
 

distance :: RegionId -> RegionId -> Int
distance (RegionId (x1, y1)) (RegionId (x2,y2)) = abs (x2 - x1) + abs (y2 - y1) 


borders :: Int -> Int -> Borders
borders x y = Borders $ fromList $ neighboor x y <$> allRegionRegionId (,) x y

findClosest :: RegionId -> [RegionId] -> RegionId
findClosest origin = minimumBy (comparing (distance origin))