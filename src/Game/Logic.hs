{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BlockArguments, ScopedTypeVariables #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds, MultiWayIf #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Game.Logic where

import Data.Aeson
import qualified Data.Map as Map
import GHC.Generics
import Region
import Data.Maybe
import PlayerManagement
import Data.List
import Data.Either
import Control.Lens
import Polysemy
import Polysemy.Reader
import Polysemy.Input
import Polysemy.Error
import Game.Effects
import Data.Foldable
import Control.Monad
import Data.Coerce
import Character.Stats
import Debug.Trace
import TileMap.Environment
 
getPlayerUnit :: Members '[ReadMapInfo, CurrentPlayerInfo, Error PlayerMoveInputError] r => Position -> Sem r TargetedCharacter
getPlayerUnit position = do
    unit <- getUnit position
    playerId <- getCurrentPlayerId
    case unit of 
        Right x
          | getFaction x == playerId -> pure x
        _ -> throw (NotPlayerOwned position)

getEmptyRegion :: Members '[ReadMapInfo, Error PlayerMoveInputError] r => Position -> Sem r EmptyRegion
getEmptyRegion position = do
    unit <- getUnit position
    case unit of
        Left x -> pure x
        _ -> throw (ExpectedEmpty position)
        
getEnemyRegion :: Members '[ReadMapInfo, CurrentPlayerInfo, Error PlayerMoveInputError] r => Position -> Sem r TargetedCharacter
getEnemyRegion position = do
    unit <- getUnit position
    playerId <- getCurrentPlayerId
    case unit of 
        Right x
          | getFaction x /= playerId -> pure x
        _ -> throw (ExpectedEnemy position)

isActionInRange :: (Region a, Region b) => a -> Action -> b -> Bool
isActionInRange a action b = 
        distance (getPosition a) (getPosition b) > range action

areAllies :: (Character a, Character b) => a -> b -> Bool
areAllies s1 s2 = getFaction s1 == getFaction s2


handlePlayerInput' :: Members '[CurrentPlayerInfo, ReadMapInfo, Error PlayerMoveInputError, UnitAction] r => Position -> ActionId -> Position -> Sem r ()
handlePlayerInput' origin actionId destination  = do
    playerUnit <- getPlayerUnit origin
    let action = getActionFromId actionId
    when (not $ isActionInRange origin action destination) (throw (InvalidActionRange origin destination))
    case action of
        Move _ _ -> do
            targetRegion <- getEmptyRegion destination
            moveCharacter playerUnit targetRegion
        Attack _ damage -> do
            targetRegion <- getEnemyRegion destination
            loseHP damage targetRegion
        AreaOfEffect _ damage relativePositions -> do
            let allPositions = destination : (fmap (movePosition destination) relativePositions)
            affectedArea <- traverse getUnit allPositions
            traverse_ (loseHP damage) $ rights affectedArea

isActionValid action _ (Left _)     = mustBeOnEmptySpace action
isActionValid action player (Right character)  
    | areAllies character player    = mustBeOnAllySpace action
    | otherwise                     = mustBeOnEnemySpace action

getActionRange :: Members '[ReadMapInfo, CurrentPlayerInfo, Error PlayerMoveInputError] r => Position -> ActionId -> Sem r [Position]
getActionRange position actionId = do
    character <- getPlayerUnit position
    let action = getActionFromId actionId
    filterM (fmap (isActionValid action character) . getUnit) $ rangeFromPosition (getPosition character) (range action)

getPossibleActions :: Members '[ReadMapInfo, CurrentPlayerInfo, Error PlayerMoveInputError] r => Position -> Sem r [Action]
getPossibleActions = fmap getActions . getPlayerUnit
