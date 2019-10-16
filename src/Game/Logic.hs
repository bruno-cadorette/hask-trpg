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

data PlayerInputType = Movement | Attack  deriving(Generic, Show)
data PlayerInput = PlayerInput { _inputType :: PlayerInputType, _origin :: RegionId, _destination :: RegionId } deriving (Generic, Show)

instance FromJSON PlayerInputType
instance ToJSON PlayerInputType

instance FromJSON PlayerInput
instance ToJSON PlayerInput


isRegionOwnedByPlayer :: Members '[CurrentPlayerInfo, ReadMapInfo] r => RegionId -> Sem r Bool
isRegionOwnedByPlayer regionId = do
    playerId <- getCurrentPlayerId
    fac <-  preview (_Just . faction)  <$> getUnit regionId
    return $ Just playerId == fac

isRegionOccupied :: Member ReadMapInfo r => RegionId -> Sem r Bool
isRegionOccupied regionId = (isNothing . preview (_Just . faction)) <$> getUnit regionId

isSoldierMovingTooMuch :: Member ReadMapInfo r => RegionId -> RegionId -> Sem r Bool
isSoldierMovingTooMuch regionId1 regionId2 = do
    maxDistance <- preview (_Just.movement) <$> getUnit regionId1
    return $ Just (distance regionId1 regionId2) <= maxDistance


areFromSameFactions :: Members '[ReadMapInfo] r => RegionId -> RegionId -> Sem r Bool
areFromSameFactions regionId1 regionId2 = do
    fac1 <- preview (_Just . faction) <$> getUnit regionId1
    fac2 <- preview (_Just . faction) <$> getUnit regionId2
    return $ fac1 == fac2
    
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM cond t f = cond >>= (\c -> if c then t else f)

soldierMove :: Members '[CurrentPlayerInfo, ReadMapInfo, Error PlayerMoveInputError, UnitAction] r => RegionId -> RegionId -> Sem r ()
soldierMove origin destination = 
    ifM (not <$> isRegionOwnedByPlayer origin) (throw (NotPlayerOwned origin)) $ 
    ifM (isRegionOccupied destination) (throw (RegionOccupied destination)) $
    ifM (isSoldierMovingTooMuch origin destination) (throw (MoveTooMuch origin)) $ 
    (move origin destination)


handlePlayerInput :: Members '[CurrentPlayerInfo, ReadMapInfo, Error PlayerMoveInputError, UnitAction] r => PlayerInput -> Sem r ()
handlePlayerInput (PlayerInput inputType origin destination) = 
    case inputType of
        Movement -> soldierMove origin destination
        _ -> pure ()