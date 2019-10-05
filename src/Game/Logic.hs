{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, BlockArguments, ScopedTypeVariables #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Game.Logic where

import Data.Aeson
import qualified Data.Map as Map
import GHC.Generics
import Region
import Data.Maybe
import PlayerManagement
import Data.List
import Control.Lens
import Polysemy
import Polysemy.Reader
import Polysemy.Input
import Polysemy.Error
import Game.Effects
import Data.Foldable
import Control.Monad

data SoldierAction = SoldierAction {_origin :: RegionId, _destination :: RegionId, _attack :: Maybe RegionId} deriving(Generic, Show)

data Move = Move {origin :: RegionId, destination :: RegionId, troops :: Army } deriving(Generic, Show)

instance FromJSON Move
instance ToJSON Move

data GameAction = 
    GameAction {movements :: [Move], playerReinforcement :: [(RegionId, Int)]} deriving(Generic, Show)
    
instance FromJSON GameAction
instance ToJSON GameAction

type MoveEffects = '[UpdateRegion, Error PlayerMoveInputError, ReadMapInfo, CurrentPlayerInfo] 

isRegionOwnedByPlayer :: Members '[CurrentPlayerInfo, ReadMapInfo] r => RegionId -> Sem r Bool
isRegionOwnedByPlayer regionId = do
    playerId <- getCurrentPlayerId
    fac <- view faction <$> getRegionInfo regionId
    return $ Just playerId == fac

isRegionOccupied :: Member ReadMapInfo r => RegionId -> Sem r Bool
isRegionOccupied regionId = (isNothing . view faction) <$> getRegionInfo regionId

isSoldierMovingTooMuch :: Member ReadMapInfo r => RegionId -> RegionId -> Sem r Bool
isSoldierMovingTooMuch regionId1 regionId2 = do
    maxDistance <- preview (currentSoldier._Just.movement) <$> getRegionInfo regionId1
    return $ Just (distance regionId1 regionId2) <= maxDistance

hasCapacityToMove :: Members '[ReadMapInfo] r => RegionId -> Army -> Sem r Bool
hasCapacityToMove regionId army = do
    pop <- view population <$> getRegionInfo regionId
    return (pop >= army)

areFromSameFactions :: Members '[ReadMapInfo] r => RegionId -> RegionId -> Sem r Bool
areFromSameFactions regionId1 regionId2 = do
    region1 <- getRegionInfo regionId1
    region2 <- getRegionInfo regionId2
    return $ region1^.faction == region2^.faction

reinforce :: Members '[UpdateRegion] r => RegionId -> RegionId -> Army -> Sem r ()
reinforce originId destinationId army = do
    removeTroops originId army
    addTroops destinationId army

attack :: Members '[UpdateRegion, ReadMapInfo, CurrentPlayerInfo] r => RegionId -> RegionId -> Army -> Sem r ()
attack region1 region2 army = do
    removeTroops region1 army
    population2 <- view population <$> getRegionInfo region2
    if army > population2 then do
        removeTroops region2 population2
        playerId <- getCurrentPlayerId
        changeFaction region2 playerId
        addTroops region2 (army - population2)
    else
        removeTroops region2 army
        

addTroops :: Members '[UpdateRegion] r => RegionId -> Army -> Sem r ()
addTroops regionId army = updatePopulation regionId army

removeTroops :: Members '[UpdateRegion] r => RegionId -> Army -> Sem r ()
removeTroops regionId army = updatePopulation regionId (-army)

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM cond t f = cond >>= (\c -> if c then t else f)

handleMove :: Members '[UpdateRegion, Error PlayerMoveInputError, ReadMapInfo, CurrentPlayerInfo] r => Move -> Sem r ()
handleMove (Move origin destination attackingTroopsNumber) =
    ifM (not <$> isRegionOwnedByPlayer origin) (throw (NotPlayerOwned origin)) $
    ifM (not <$> hasCapacityToMove origin attackingTroopsNumber) (throw (MoveTooMuch origin)) $
    ifM (areFromSameFactions origin destination) (reinforce origin destination attackingTroopsNumber) $
    (attack origin destination attackingTroopsNumber)

soldierMove :: Members '[CurrentPlayerInfo, ReadMapInfo, Error PlayerMoveInputError, UnitAction] r => SoldierAction -> Sem r ()
soldierMove (SoldierAction origin destination _) = 
    ifM (not <$> isRegionOwnedByPlayer origin) (throw (NotPlayerOwned origin)) $ 
    ifM (isRegionOccupied destination) (throw (RegionOccupied destination)) $
    ifM (isSoldierMovingTooMuch origin destination) (throw (MoveTooMuch origin)) $ 
    (moveM origin destination)

handleReinforcement :: Members '[CurrentPlayerInfo, UpdateRegion, ReadMapInfo, Error PlayerMoveInputError] r => [(RegionId, Int)] -> Sem r ()
handleReinforcement xs = do
    maxReinforcement <- getMaxReinforcement
    if (sum $ fmap snd xs) > maxReinforcement then
        throw TooMuchReinforcement
    else
        traverse_ (\(regionId, newTroops) -> 
            ifM (isRegionOwnedByPlayer regionId) 
                (addTroops regionId (Army newTroops))
                (throw (NotPlayerOwned regionId))) xs
        