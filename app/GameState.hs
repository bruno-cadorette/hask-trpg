{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds #-}

module GameState where

import Data.Aeson
import qualified Data.Map as Map
import GHC.Generics
import Region
import Data.Maybe
import PlayerManagement
import Data.List
import Control.Monad.Except
import Control.Monad.Reader
import Polysemy
import Polysemy.Error

data ReadDb m a where
    GetRegionInfo :: RegionId -> ReadDb m RegionInfo

data UpdateRegion m a where
    UpdatePopulation :: RegionId -> Army -> UpdateRegion m ()
    ChangeFaction :: RegionId -> PlayerId -> Army -> UpdateRegion m ()

data PlayerInformation m a where
    GetCurrentPlayerId :: PlayerInformation m PlayerId


makeSem ''ReadDb
makeSem ''UpdateRegion
makeSem ''PlayerInformation

data DbError = 
    OutOfBound RegionId

data PlayerMoveInputError =
    SelectedEmptySource RegionId |
    MoveTooMuch RegionId Army Army
    deriving (Show)


data Move = Move {origin :: RegionId, destination :: RegionId, troops :: Army } deriving(Generic, Show)

instance FromJSON Move
instance ToJSON Move

data GameAction = 
    GameAction {movements :: [Move], playerReinforcement :: [(RegionId, Int)]} deriving(Generic, Show)
    

data AttackingArmy = AttackingArmy PlayerId Army deriving (Show)


instance FromJSON GameAction
instance ToJSON GameAction

type MoveEffects = '[UpdateRegion, Error PlayerMoveInputError, ReadDb, PlayerInformation] 

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



reinforce :: GameMap -> GameMap
reinforce = GameMap . Map.map (\(RegionInfo f p) -> RegionInfo f (if f == Nothing then p else p + 1)) . gameMapToMap

getNextReinforcement :: GameMap -> [(PlayerId, Int)]
getNextReinforcement = fmap (\xs -> (head xs, length xs)) . group . sort . mapMaybe faction . Map.elems . gameMapToMap

