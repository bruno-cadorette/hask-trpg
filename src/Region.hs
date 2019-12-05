{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, BlockArguments, TypeApplications, TypeFamilies #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds, RankNTypes, ScopedTypeVariables, MultiParamTypeClasses #-}
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
import TileMap.Environment
import Control.Applicative


newtype UnitPositions = UnitPositions (Map RegionId CharacterUnit) deriving (FromJSON, ToJSON, Show)


--newtype UnitPositions = UnitPositions (Map RegionId SoldierUnit) deriving (FromJSON, ToJSON, Show)

data Game = Game { _gameBorders :: Borders, _turnNumber :: Int, _unitPositions :: UnitPositions} deriving (Show)


makeLenses ''Game

class Region a where
    regionId :: a -> RegionId

instance Region RegionId where
    regionId = id

instance Region EmptyRegion where
    regionId (EmptyRegion regionId _) = regionId

data EmptyRegion = EmptyRegion RegionId TerrainType deriving(Show)

data TargetedCharacter = TargetedCharacter RegionId CharacterUnit

instance Region TargetedCharacter where
    regionId (TargetedCharacter r _) = r

instance Character TargetedCharacter where
    getAttackRange (TargetedCharacter _ c) = attack c
    getCurrentHp (TargetedCharacter _ c) = hp c
    getFaction (TargetedCharacter _ c) = faction c
    inWeaponRange i (TargetedCharacter _ c) = inWeaponRange i c
        
{-
canWalk :: TargetSoldier k -> Maybe (TargetSoldier Walking)
canWalk s@(TargetSoldier a b) = 
    case soldier s^.movementType of
        Walking -> Just $ (TargetSoldier a b)
        _ -> Nothing

isGrass :: EmptyRegion k -> Maybe (EmptyRegion Grass)
isGrass (EmptyRegion r t) =
    case t of
        Grass -> Just (EmptyRegion r t)
        _ -> Nothing


soldierCanMove :: TargetSoldier y -> EmptyRegion x -> Maybe (TargetSoldier Walking, EmptyRegion Grass)
soldierCanMove soldier region = 
    liftA2 (,) (canWalk soldier) (isGrass region)
-}

data UnitAction m a where
    LoseHP :: Int -> TargetedCharacter -> UnitAction m () 
    --Move :: CanMoveTo movementType terrainType => TargetSoldier movementType -> EmptyRegion terrainType -> UnitAction m ()
    Move :: TargetedCharacter -> EmptyRegion -> UnitAction m ()

makeSem ''UnitAction

data ReadMapInfo m a where
    GetUnit :: RegionId -> ReadMapInfo m (Either EmptyRegion TargetedCharacter)

makeSem ''ReadMapInfo

modifyUnitPositions :: Member (State Game) r => (UnitPositions -> UnitPositions) -> Sem r ()
modifyUnitPositions = modify . over unitPositions 

runUnitMoving ::Members '[State Game] r => InterpreterFor UnitAction r
runUnitMoving = interpret $ \case
    Move (TargetedCharacter orign playerToMove) (EmptyRegion destination _) -> do
        modifyUnitPositions (coerce (Data.Map.delete orign . Data.Map.insert destination playerToMove))
    LoseHP damage (TargetedCharacter location _) -> 
        modifyUnitPositions (coerce (Data.Map.update (hitCharacter damage) location))


runReadMapInfo :: Members '[State Game] r => InterpreterFor ReadMapInfo r
runReadMapInfo = interpret $ \(GetUnit regionId) -> 
    gets (
        maybe (Left $ EmptyRegion regionId Grass) (Right . TargetedCharacter regionId). 
        Data.Map.lookup regionId . 
        coerce . 
        view unitPositions)

baseUnitPositions :: UnitPositions
baseUnitPositions = UnitPositions $ fromList [(RegionId (2,2), strongKnight (PlayerId 1)), (RegionId (6,8), baseKnight (PlayerId 2)) ]
 
distance :: RegionId -> RegionId -> Int
distance (RegionId (x1, y1)) (RegionId (x2,y2)) = abs (x2 - x1) + abs (y2 - y1) 

findClosest :: RegionId -> [RegionId] -> RegionId
findClosest origin = minimumBy (comparing (distance origin))
