{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, BlockArguments, ScopedTypeVariables #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds #-}

module Game.Logic where

import Data.Aeson
import qualified Data.Map as Map
import GHC.Generics
import Region
import Data.Maybe
import PlayerManagement
import Data.List
import Polysemy
import Polysemy.Reader
import Polysemy.Input
import Polysemy.Error
import Game.Effects
import Data.Foldable


data Move = Move {origin :: RegionId, destination :: RegionId, troops :: Army } deriving(Generic, Show)

instance FromJSON Move
instance ToJSON Move

data GameAction = 
    GameAction {movements :: [Move], playerReinforcement :: [(RegionId, Int)]} deriving(Generic, Show)
    

data AttackingArmy = AttackingArmy PlayerId Army deriving (Show)
getCurrentPlayerId :: Member (Reader PlayerId) r => Sem r PlayerId
getCurrentPlayerId = ask

instance FromJSON GameAction
instance ToJSON GameAction

type MoveEffects = '[UpdateRegion, Error PlayerMoveInputError, ReadMapInfo, Reader PlayerId] 

updateAttackingRegion :: Members '[UpdateRegion, Error PlayerMoveInputError] r => Move -> RegionInfo -> Sem r ()
updateAttackingRegion (Move originId _ troopsToMove) (RegionInfo _ baseTroops) =   
    if baseTroops >= troopsToMove then
        updatePopulation originId (baseTroops - troopsToMove)
    else
        throw (MoveTooMuch originId baseTroops troopsToMove)

extractAttackingArmy :: Members MoveEffects r => Move -> Sem r AttackingArmy 
extractAttackingArmy movement@(Move originId _ troops) = do
    regionInfo <- getRegionInfo originId
    playerId <- getCurrentPlayerId
    updateAttackingRegion movement regionInfo
    return $ AttackingArmy playerId troops

handleMove :: Members MoveEffects r => Move -> Sem r ()
handleMove move@(Move originId destinationId attackingTroopsNumber) = do 
    attackingArmy <- extractAttackingArmy move 
    regionInfo <- getRegionInfo destinationId
    invasion attackingArmy destinationId regionInfo
    
invasion :: Member UpdateRegion r => AttackingArmy -> RegionId -> RegionInfo -> Sem r ()
invasion (AttackingArmy attackerFaction attackerTroops) regionId (RegionInfo defenderFaction defenderTroops)
    | (Just attackerFaction) == defenderFaction =
        updatePopulation regionId (attackerTroops + defenderTroops)
    | defenderTroops - attackerTroops < 0 =
        changeFaction regionId attackerFaction (attackerTroops - defenderTroops)
    | otherwise =
        updatePopulation regionId (defenderTroops - attackerTroops)


updateGame playerId game moves = do
    runGameTurn playerId game $ traverse_ handleMove moves

reinforce :: GameMap -> GameMap
reinforce = Map.map (\(RegionInfo f p) -> RegionInfo f (if f == Nothing then p else p + 1))

getNextReinforcement :: GameMap -> [(PlayerId, Int)]
getNextReinforcement = fmap (\xs -> (head xs, length xs)) . group . sort . mapMaybe _faction . Map.elems

