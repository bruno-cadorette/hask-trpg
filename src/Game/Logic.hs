{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BlockArguments, ScopedTypeVariables #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds, MultiWayIf #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Game.Logic where

import Data.Bifunctor
import Data.Either
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
import Data.Coerce
import Soldier
import Debug.Trace

data PlayerInputType = Movement | Attack  deriving(Generic, Show)
data PlayerInput = PlayerInput { _inputType :: PlayerInputType, _origin :: RegionId, _destination :: RegionId } deriving (Generic, Show)
data PossibleInput = PossibleInput PlayerInputType [RegionId]

getPossibleInputs :: Members '[ReadMapInfo, CurrentPlayerInfo, Error PlayerMoveInputError] r => RegionId -> Sem r [PossibleInput]
getPossibleInputs regionId = do
  unit <- getPlayerUnit regionId
  (emptyRegions, occupiedRegions) <- fmap partitionEithers $ traverse getUnit $ extendedNeighboor ((soldier unit)^.range) regionId
  let enemyRegions = filter (areAllies unit) occupiedRegions
  return $ 
    (if null emptyRegions then [] else [PossibleInput Movement (fmap getRegionId emptyRegions)]) ++ 
    (if null enemyRegions then [] else [PossibleInput Attack (fmap getRegionId enemyRegions)])

getPlayerUnit :: Members '[ReadMapInfo, CurrentPlayerInfo, Error PlayerMoveInputError] r => RegionId -> Sem r TargetSoldier
getPlayerUnit regionId = do
    unit <- getUnit regionId
    playerId <- getCurrentPlayerId
    case unit of 
        Right x
          | (soldier x)^.faction == playerId -> pure x
        _ -> throw (NotPlayerOwned regionId)

isSoldierMovingTooMuch :: (Region a, Region b, Soldier a) => a -> b -> Bool
isSoldierMovingTooMuch a b = 
    distance (getRegionId a) (getRegionId b) > (soldier a)^.movement


isSoldierInRange :: (Soldier a, Region a, Region b) => a -> b -> Bool
isSoldierInRange a b = 
    distance (getRegionId a) (getRegionId b) <= (soldier a)^.range

areAllies :: (Soldier a, Soldier b) => a -> b -> Bool
areAllies s1 s2 = (soldier s1)^.faction == (soldier s2)^.faction 

soldierMove :: Members '[Error PlayerMoveInputError, UnitAction] r => TargetSoldier -> EmptyRegion -> Sem r ()
soldierMove soldier emptyRegion 
    | isSoldierMovingTooMuch soldier emptyRegion = throw (MoveTooMuch $ getRegionId soldier )
    | otherwise = move soldier emptyRegion

soldierAttack :: Members '[Error PlayerMoveInputError, UnitAction] r => TargetSoldier -> TargetSoldier -> Sem r ()
soldierAttack attacker defender
    | areAllies attacker defender = throw (AttackAllies (getRegionId attacker) (getRegionId defender))
    | not $ isSoldierInRange attacker defender = throw (AttackTooFar (getRegionId attacker) (getRegionId defender))
    | otherwise = loseHP ((soldier attacker)^.attack) defender


handlePlayerInput :: Members '[CurrentPlayerInfo, ReadMapInfo, Error PlayerMoveInputError, UnitAction] r => PlayerInput -> Sem r ()
handlePlayerInput (PlayerInput inputType origin destination) = do
    playerUnit <- traceShow origin $ getPlayerUnit origin
    targetRegion <- traceShow destination $ getUnit destination
    case targetRegion of
        Right soldier -> soldierAttack playerUnit soldier
        Left emptyRegion -> soldierMove playerUnit emptyRegion