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
import Character.Stats
import Data.Coerce
import Debug.Trace
import TileMap.Environment
import Control.Applicative


newtype UnitPositions = UnitPositions (Map Position CharacterUnit) deriving (FromJSON, ToJSON, Show)


--newtype UnitPositions = UnitPositions (Map Position SoldierUnit) deriving (FromJSON, ToJSON, Show)

data Game = Game { _gameBorders :: Borders, _turnNumber :: Int, _unitPositions :: UnitPositions} deriving (Show)


makeLenses ''Game

class Region a where
    getPosition :: a -> Position

instance Region Position where
    getPosition = id

instance Region EmptyRegion where
    getPosition (EmptyRegion position _) = position

data EmptyRegion = EmptyRegion Position TerrainType deriving(Show)

data TargetedCharacter = TargetedCharacter Position CharacterUnit

instance Region TargetedCharacter where
    getPosition (TargetedCharacter r _) = r

instance Character TargetedCharacter where
    getAttackRange (TargetedCharacter _ c) = getAttackRange c
    getAttackDamage (TargetedCharacter _ c) = getAttackDamage c
    getCurrentHp (TargetedCharacter _ c) = getCurrentHp c
    getFaction (TargetedCharacter _ c) = getFaction c
    getMovementRange (TargetedCharacter _ c) = getMovementRange c
    getActions (TargetedCharacter _ c) = getActions c
        
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
    MoveCharacter :: TargetedCharacter -> EmptyRegion -> UnitAction m ()

makeSem ''UnitAction

data ReadMapInfo m a where
    GetUnit :: Position -> ReadMapInfo m (Either EmptyRegion TargetedCharacter)
    GetAllTiles :: ReadMapInfo m Borders

makeSem ''ReadMapInfo

modifyUnitPositions :: Member (State Game) r => (UnitPositions -> UnitPositions) -> Sem r ()
modifyUnitPositions = modify . over unitPositions 

runUnitMoving ::Members '[State Game] r => InterpreterFor UnitAction r
runUnitMoving = interpret $ \case
    MoveCharacter (TargetedCharacter orign playerToMove) (EmptyRegion destination _) -> do
        modifyUnitPositions (coerce (Data.Map.delete orign . Data.Map.insert destination playerToMove))
    LoseHP damage (TargetedCharacter location _) -> 
        modifyUnitPositions (coerce (Data.Map.update (hitCharacter damage) location))


runReadMapInfo :: Members '[State Game] r => InterpreterFor ReadMapInfo r
runReadMapInfo = interpret $ \case
    (GetUnit regionId) -> 
        gets (
            maybe (Left $ EmptyRegion regionId Grass) (Right . TargetedCharacter regionId). 
            Data.Map.lookup regionId . 
            coerce . 
            view unitPositions)
    GetAllTiles -> gets _gameBorders
    
baseUnitPositions :: UnitPositions
baseUnitPositions = UnitPositions $ fromList [(Position (2,2), strongKnight (PlayerId 1)), (Position (6,8), baseKnight (PlayerId 2)) ]
 
distance :: Position -> Position -> Int
distance (Position (x1, y1)) (Position (x2,y2)) = abs (x2 - x1) + abs (y2 - y1) 

findClosest :: Position -> [Position] -> Position
findClosest origin = minimumBy (comparing (distance origin))


rangeFromPosition :: Position -> Int -> [Position]
rangeFromPosition region n = findRegions region n []
    where
        findRegions :: Position -> Int -> [Position] -> [Position]
        findRegions r 0 acc = acc
        findRegions r n acc = 
            concatMap (\x -> findRegions x (n - 1) (x : acc)) $ 
            Data.List.filter (\x -> notElem x acc) $ 
            directNeighboor r
