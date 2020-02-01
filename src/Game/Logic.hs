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

isSoldierMovingTooMuch :: (Region a, Region b, Character a) => a -> b -> Bool
isSoldierMovingTooMuch a b = 
    distance (getPosition a) (getPosition b) > getMovementRange a


isSoldierInRange :: (Character a, Region a, Region b) => a -> b -> Bool
isSoldierInRange a b = 
    distance (getPosition a) (getPosition b) > getAttackRange a

areAllies :: (Character a, Character b) => a -> b -> Bool
areAllies s1 s2 = getFaction s1 == getFaction s2

soldierMove :: Members '[Error PlayerMoveInputError, UnitAction] r => TargetedCharacter -> EmptyRegion -> Sem r ()
soldierMove soldier emptyRegion 
    | isSoldierMovingTooMuch soldier emptyRegion = throw (MoveTooMuch $ getPosition soldier )
    | otherwise = moveCharacter soldier emptyRegion

soldierAttack :: Members '[Error PlayerMoveInputError, UnitAction] r => TargetedCharacter -> TargetedCharacter -> Sem r ()
soldierAttack attacker defender
    | areAllies attacker defender = throw (AttackAllies (getPosition attacker) (getPosition defender))
    | not $ isSoldierInRange attacker defender = throw (AttackTooFar (getPosition attacker) (getPosition defender))
    | otherwise = loseHP (getAttackDamage attacker) defender


handlePlayerInput' :: Members '[CurrentPlayerInfo, ReadMapInfo, Error PlayerMoveInputError, UnitAction] r => Position -> Action -> Position -> Sem r ()
handlePlayerInput' origin inputType destination  = do
    playerUnit <- getPlayerUnit origin
    case inputType of
        Move -> do
            targetRegion <- getEmptyRegion destination
            soldierMove playerUnit targetRegion
        _ -> do
            targetRegion <- getEnemyRegion destination
            soldierAttack playerUnit targetRegion


possibleMoves :: Member ReadMapInfo r => TargetedCharacter -> Sem r [Position]
possibleMoves character = do
    filterM (fmap isLeft . getUnit) $ rangeFromPosition (getPosition character) (getMovementRange character)
     
possibleAttacks :: Member ReadMapInfo r => TargetedCharacter -> Sem r [Position]
possibleAttacks character = 
    filterM (fmap keepRegion . getUnit) $ rangeFromPosition (getPosition character) (getAttackRange character)
    where 
        keepRegion (Right otherChar) = not $ areAllies character otherChar
        keepRegion _ = False

findActionRange Move = possibleMoves
findActionRange _ = possibleAttacks

getActionRanges characterPosition action = do
    playerUnit <- getPlayerUnit characterPosition
    findActionRange action playerUnit

getPossibleActions :: Members '[ReadMapInfo, CurrentPlayerInfo, Error PlayerMoveInputError] r => Position -> Sem r [Action]
getPossibleActions = fmap getActions . getPlayerUnit
