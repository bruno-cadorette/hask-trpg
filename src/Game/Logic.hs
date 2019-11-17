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

data PlayerInputType = Movement | Attack  deriving(Generic, Show)
data PlayerInput = PlayerInput { _inputType :: PlayerInputType, _origin :: RegionId, _destination :: RegionId } deriving (Generic, Show)

instance FromJSON PlayerInputType
instance ToJSON PlayerInputType

instance FromJSON PlayerInput
instance ToJSON PlayerInput
 
getPlayerUnit :: Members '[ReadMapInfo, CurrentPlayerInfo] r => RegionId -> Sem r (Maybe TargetSoldier)
getPlayerUnit regionId = do
    cond <- isRegionOwnedByPlayer regionId
    if cond then 
        getTargetSoldier regionId
    else
        pure Nothing

getEmptyRegion :: Member ReadMapInfo r => RegionId -> Sem r (Maybe EmptyRegion)
getEmptyRegion regionId = do
    unit <- getUnit regionId
    pure $ if isJust unit then
        Nothing
    else 
        Just $ EmptyRegion regionId

getFaction :: Member ReadMapInfo r => RegionId -> Sem r (Maybe PlayerId)
getFaction = fmap (preview  (_Just . faction)) . getUnit

isRegionOwnedByPlayer :: Members '[CurrentPlayerInfo, ReadMapInfo] r => RegionId -> Sem r Bool
isRegionOwnedByPlayer regionId = do
    playerId <- getCurrentPlayerId
    fac <-  getFaction regionId
    return $ Just playerId == fac

isSoldierMovingTooMuch :: (Region a, Region b, Soldier a) => a -> b -> Bool
isSoldierMovingTooMuch a b = 
    distance (regionId a) (regionId b) > (soldier a)^.movement


isSoldierInRange :: (Soldier a, Region a, Region b) => a -> b -> Bool
isSoldierInRange a b = 
    distance (regionId a) (regionId b) <= (soldier a)^.range

areAllies :: (Soldier a, Soldier b) => a -> b -> Bool
areAllies s1 s2 = (soldier s1)^.faction == (soldier s2)^.faction 
    

soldierMove :: Members '[CurrentPlayerInfo, ReadMapInfo, Error PlayerMoveInputError, UnitAction] r => RegionId -> RegionId -> Sem r ()
soldierMove origin destination = do 
    playerSoldier <- getPlayerUnit origin
    emptyRegion <- getEmptyRegion destination
    case (playerSoldier, emptyRegion) of
        (Nothing, _) -> throw (NotPlayerOwned origin)
        (_, Nothing) -> throw (RegionOccupied destination)
        (Just soldier, Just emptyRegion) -> if
            | isSoldierMovingTooMuch soldier emptyRegion -> throw (MoveTooMuch origin)
            | otherwise -> move soldier emptyRegion

soldierAttack :: Members '[CurrentPlayerInfo, ReadMapInfo, Error PlayerMoveInputError, UnitAction] r => RegionId -> RegionId -> Sem r ()
soldierAttack origin destination = do
    attacker <- getPlayerUnit origin
    defender <- getTargetSoldier destination
    case (attacker, defender) of
        (Nothing, _) -> throw (NotPlayerOwned origin)
        (_, Nothing) -> throw (RegionNotOccupied destination)
        (Just attacker, Just defender) -> if
            | areAllies attacker defender -> throw (AttackAllies origin destination)
            | not $ isSoldierInRange attacker defender -> throw (AttackTooFar origin destination)
            | otherwise -> loseHP ((soldier attacker)^.attack) defender


handlePlayerInput :: Members '[CurrentPlayerInfo, ReadMapInfo, Error PlayerMoveInputError, UnitAction] r => PlayerInput -> Sem r ()
handlePlayerInput (PlayerInput inputType origin destination) = 
    case inputType of
        Movement -> soldierMove origin destination
        Attack -> soldierAttack origin destination