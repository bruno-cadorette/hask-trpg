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
import Data.Ord
import Polysemy
import Polysemy.State
import Soldier
import Data.Coerce
import Debug.Trace
import Servant
import qualified Data.Text as Text



newtype RegionId = RegionId (Int, Int) deriving (Show, Eq, Ord, ToJSONKey, FromJSONKey)

instance FromHttpApiData RegionId where
  parseQueryParam txt = 
    case Text.splitOn "_" txt of
      [x, y] -> Right $ RegionId (read (Text.unpack x), read (Text.unpack y))
      _ -> Left "Cannot parse RegionId"

newtype Borders = Borders (Map RegionId [RegionId]) deriving (Show)

newtype UnitPositions = UnitPositions (Map RegionId SoldierUnit) deriving (Show)

data Game = Game { _gameBorders :: Borders, _turnNumber :: Int, _unitPositions :: UnitPositions} deriving (Show)

makeLenses ''Game



newtype Army = Army Int deriving (Show, Eq, Ord, Num)
class Region a where
    getRegionId :: a -> RegionId

instance Region RegionId where
    getRegionId = id

instance Region EmptyRegion where
    getRegionId (EmptyRegion regionId) = regionId

instance Region TargetSoldier where
    getRegionId (TargetSoldier regionId _) = regionId
    

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
    GetUnit :: RegionId -> ReadMapInfo m (Either EmptyRegion TargetSoldier)

makeSem ''ReadMapInfo

modifyUnitPositions :: Member (State Game) r => (UnitPositions -> UnitPositions) -> Sem r ()
modifyUnitPositions = modify . over unitPositions 

runUnitMoving ::Members '[State Game] r => InterpreterFor UnitAction r
runUnitMoving = interpret $ \case
    Move (TargetSoldier orign playerToMove) (EmptyRegion destination) -> do
        modifyUnitPositions (coerce (Data.Map.delete orign . Data.Map.insert destination playerToMove))
    LoseHP damage (TargetSoldier location _) -> 
        modifyUnitPositions (coerce (Data.Map.update (hitSoldier damage) location))


runReadMapInfo :: Members '[State Game] r => InterpreterFor ReadMapInfo r
runReadMapInfo = interpret $ \(GetUnit regionId) -> 
    gets (
        maybe (Left $ EmptyRegion regionId) (Right . TargetSoldier regionId). 
        Data.Map.lookup regionId . 
        coerce . 
        view unitPositions)


baseUnitPositions :: UnitPositions
baseUnitPositions = UnitPositions $ fromList [(RegionId (2,2), strongSoldier 0 (PlayerId 1)), (RegionId (6,8), baseSoldier 1 (PlayerId 2)) ]

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

directNeighboor :: RegionId -> [RegionId]
directNeighboor (RegionId (a,b)) = [RegionId (a + 1, b), RegionId (a - 1, b), RegionId (a, b + 1), RegionId (a, b - 1)]

borders :: Int -> Int -> Borders
borders x y = Borders $ fromList $ neighboor x y <$> allRegionRegionId (,) x y

findClosest :: RegionId -> [RegionId] -> RegionId
findClosest origin = minimumBy (comparing (distance origin))

extendedNeighboor n r = tail $ nub $ concat $ Data.List.take n $ iterate (concatMap directNeighboor) [r]