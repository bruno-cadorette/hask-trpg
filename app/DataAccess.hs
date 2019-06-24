{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, BlockArguments, TypeApplications, TypeFamilies #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds, RankNTypes, ScopedTypeVariables #-}

module DataAccess where

import Polysemy.State
import Servant
import Data.List
import Data.Maybe
import GHC.Generics
import Data.Map 
import Data.Aeson (FromJSON, ToJSON)
import Data.Foldable
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Region
import GameState
import PlayerManagement
import Control.Lens
import Polysemy
import Polysemy.Error
import Polysemy.Reader

data TurnInfo = TurnInfo { _gameMap :: GameMap, _reinforcement :: Map PlayerId Int, _turnNumber :: Int }

makeLenses ''TurnInfo

data Game = Game { _gameBorders :: Borders, _turnInfo :: TurnInfo}

makeLenses ''Game

type GameHub = Map GameId (TVar Game)

newtype GameId = GameId Integer deriving (Num, Eq, Ord, Show, FromHttpApiData, Generic, ToJSON)

newtype RiskyT a = RiskyT (Identity a)
    deriving (Functor, Applicative, Monad)

data ReadGameManagement m a where
    GetGame :: GameId -> ReadGameManagement m Game


updateGame playerId game moves = do
    runGameTurn playerId game $ traverse_ handleMove moves
    

runGameTurn :: 
    PlayerId -> 
    TVar Game ->      
    Sem '[
        UpdateRegion, ReadMapInfo, State GameMap,
        Error PlayerMoveInputError, Reader (TVar Game), Reader PlayerId,
        Lift STM] a ->
    STM (Either PlayerMoveInputError a)
runGameTurn playerId game = 
    runM . 
    runReader @PlayerId playerId . 
    runReader @(TVar Game) game . 
    runError .
    runGameMapState .
    runReadMapInfo . 
    runUpdateRegion



runReadMapInfo :: Members '[State GameMap, Error PlayerMoveInputError] r => Sem (ReadMapInfo ': r) a -> Sem r a
runReadMapInfo = interpret $ \(GetRegionInfo regionId) -> do
    lookupResult <- gets (Data.Map.lookup regionId)
    case lookupResult of
        Just region -> return region
        Nothing -> throw $ RegionDontExist regionId


runUpdateRegion:: Members '[State GameMap] r => Sem (UpdateRegion ': r) a -> Sem r a
runUpdateRegion = interpret $ \case
    (UpdatePopulation regionId newPops) ->
        modify (adjust (set population newPops) regionId)
    (ChangeFaction regionId newFac newPops) -> 
        modify (Data.Map.insert regionId (RegionInfo (Just newFac) newPops))


runGameMapState :: Members '[Lift STM, Reader (TVar Game)] r => Sem (State GameMap ': r) a -> Sem r a
runGameMapState = interpret $ \case
    Get   -> do
        var <- ask
        sendM $ fmap (view (turnInfo.gameMap)) $ readTVar var
    Put s -> do
        var <- ask
        sendM $ modifyTVar var (set (turnInfo.gameMap) s)    