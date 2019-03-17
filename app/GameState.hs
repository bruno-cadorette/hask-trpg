{-# LANGUAGE DeriveGeneric #-}
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

type GameStateMonad a = ReaderT GameMap (Except GameStateError) a

data GameStateError = 
    MoveFromEmpty RegionId |
    MoveTooMuch RegionId Army Army |
    OutOfBound RegionId
    deriving (Show)

data GameAction = 
    Move {origin :: RegionId, destination :: RegionId, troops :: Army } deriving(Generic, Show)

data AttackingArmy = AttackingArmy PlayerId Army deriving (Show)

data UpdateRegionCommand = UpdateRegionPopulation RegionId Army | ChangeRegionFaction RegionId PlayerId Army

instance FromJSON GameAction
instance ToJSON GameAction


askGameMap = ask

liftMaybe _ (Just a) = pure a
liftMaybe b _        = throwError b

getRegionInfo :: RegionId -> GameStateMonad RegionInfo
getRegionInfo regionId = do
    (GameMap gameMap) <- askGameMap
    liftMaybe (OutOfBound regionId) (Map.lookup regionId gameMap)


getPlayerId :: RegionId -> RegionInfo -> GameStateMonad PlayerId
getPlayerId originId (RegionInfo playerId _) =
    liftMaybe (MoveFromEmpty originId) playerId

updateAttackingRegion :: GameAction -> RegionInfo -> GameStateMonad UpdateRegionCommand
updateAttackingRegion (Move originId _ troopsToMove) (RegionInfo _ baseTroops) =   
    if baseTroops >= troopsToMove then
        pure $ UpdateRegionPopulation originId (baseTroops - troopsToMove)
    else
        throwError (MoveTooMuch originId baseTroops troopsToMove)

attackerCommand :: GameAction -> GameStateMonad (AttackingArmy, UpdateRegionCommand)
attackerCommand movement@(Move originId _ troops) = do
    regionInfo <- getRegionInfo originId
    playerId <- getPlayerId originId regionInfo
    updateCommand <- updateAttackingRegion movement regionInfo
    return (AttackingArmy playerId troops, updateCommand)

defenderCommand :: AttackingArmy -> RegionId -> GameStateMonad UpdateRegionCommand
defenderCommand attackingArmy destinationId = invasion attackingArmy destinationId <$> getRegionInfo destinationId

handleMove :: GameAction -> GameStateMonad [UpdateRegionCommand]
handleMove move@(Move originId destinationId attackingTroopsNumber) = do 
    (attackers, updateAttack) <- attackerCommand move 
    updateDefense <- defenderCommand attackers destinationId
    return $ [updateAttack, updateDefense] 
    
invasion :: AttackingArmy -> RegionId -> RegionInfo -> UpdateRegionCommand
invasion (AttackingArmy attackerFaction attackerTroops) regionId (RegionInfo defenderFaction defenderTroops)
    | (Just attackerFaction) == defenderFaction =
        UpdateRegionPopulation regionId (attackerTroops + defenderTroops)
    | defenderTroops - attackerTroops < 0 =
        ChangeRegionFaction regionId attackerFaction (attackerTroops - defenderTroops)
    | otherwise =
        UpdateRegionPopulation regionId (defenderTroops - attackerTroops)

reinforce :: GameMap -> GameMap
reinforce = GameMap . Map.map (\(RegionInfo f p) -> RegionInfo f (if f == Nothing then p else p + 1)) . gameMapToMap

getNextReinforcement :: GameMap -> [(PlayerId, Int)]
getNextReinforcement = fmap (\xs -> (head xs, length xs)) . group . sort . mapMaybe faction . Map.elems . gameMapToMap

