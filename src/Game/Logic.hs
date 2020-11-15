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
data PlayerInput = PlayerInput { _inputType :: PlayerInputType, _origin :: Position, _destination :: Position } deriving (Generic, Show)
data PossibleInput = PossibleInput PlayerInputType [Position] deriving (Show)

getPossibleInputs :: Members '[ReadMapInfo, CurrentPlayerInfo, Error PlayerMoveInputError] r => Position -> Sem r [PossibleInput]
getPossibleInputs regionId = do
  unit <- getPlayerUnit regionId
  let getTilesInRange selector = traverse getUnit $ extendedNeighboor ((soldier unit)^.selector) regionId
  enemyRegions <- fmap getPosition <$> filter (not . areAllies unit)  <$> rights <$> (getTilesInRange range)
  emptyRegions <- lefts <$> (getTilesInRange movement)
  return $ 
    (if null emptyRegions then [] else [PossibleInput Movement (fmap getPosition emptyRegions)]) ++ 
    (if null enemyRegions then [] else [PossibleInput Attack (fmap getPosition enemyRegions)])

getPlayerUnit :: Members '[ReadMapInfo, CurrentPlayerInfo, Error PlayerMoveInputError] r => Position -> Sem r TargetSoldier
getPlayerUnit regionId = do
    unit <- getUnit regionId
    playerId <- getCurrentPlayerId
    case unit of 
        Right x
          | (soldier x)^.faction == playerId -> pure x
        _ -> throw (NotPlayerOwned regionId)

isSoldierMovingTooMuch :: (Region a, Region b, Soldier a) => a -> b -> Bool
isSoldierMovingTooMuch a b = 
    distance (getPosition a) (getPosition b) > (soldier a)^.movement


isSoldierInRange :: (Soldier a, Region a, Region b) => a -> b -> Bool
isSoldierInRange a b = 
    distance (getPosition a) (getPosition b) <= (soldier a)^.range

areAllies :: (Soldier a, Soldier b) => a -> b -> Bool
areAllies s1 s2 = (soldier s1)^.faction == (soldier s2)^.faction 

soldierMove :: Members '[Error PlayerMoveInputError, UnitAction] r => TargetSoldier -> EmptyRegion -> Sem r ()
soldierMove soldier emptyRegion 
    | isSoldierMovingTooMuch soldier emptyRegion = throw (MoveTooMuch $ getPosition soldier )
    | otherwise = move soldier emptyRegion

soldierAttack :: Members '[Error PlayerMoveInputError, UnitAction] r => TargetSoldier -> TargetSoldier -> Sem r ()
soldierAttack attacker defender
    | areAllies attacker defender = throw (AttackAllies (getPosition attacker) (getPosition defender))
    | not $ isSoldierInRange attacker defender = throw (AttackTooFar (getPosition attacker) (getPosition defender))
    | otherwise = loseHP ((soldier attacker)^.attack) defender


handlePlayerInput :: Members '[CurrentPlayerInfo, ReadMapInfo, Error PlayerMoveInputError, UnitAction] r => PlayerInput -> Sem r ()
handlePlayerInput (PlayerInput inputType origin destination) = do
    playerUnit <- traceShow origin $ getPlayerUnit origin
    targetRegion <- traceShow destination $ getUnit destination
    case targetRegion of
        Right soldier -> soldierAttack playerUnit soldier
        Left emptyRegion -> soldierMove playerUnit emptyRegion