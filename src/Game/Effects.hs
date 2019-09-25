{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, BlockArguments, TypeApplications, TypeFamilies #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds, RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Game.Effects where

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
import PlayerManagement
import Control.Lens
import Control.Exception.Base(Exception)
import Polysemy
import Polysemy.Error
import Polysemy.Reader
import Polysemy.Input
import Servant.Checked.Exceptions
import Network.HTTP.Types.Status

data TurnInfo = TurnInfo { _gameMap :: GameMap, _maxReinforcement :: Map PlayerId Int, _turnNumber :: Int }

makeLenses ''TurnInfo

data Game = Game { _gameBorders :: Borders, _turnInfo :: TurnInfo}

makeLenses ''Game

type GameHub = Map GameId (TVar Game)

newtype GameId = GameId Integer deriving (Num, Eq, Ord, Show, FromHttpApiData, Generic, ToJSON)

data ReadMapInfo m a where
    GetRegionInfo :: RegionId -> ReadMapInfo m RegionInfo


data CurrentPlayerInfo m a where
    GetCurrentPlayerId :: CurrentPlayerInfo m PlayerId
    GetMaxReinforcement :: CurrentPlayerInfo m Int

data UpdateRegion m a where
    UpdatePopulation :: RegionId -> Army -> UpdateRegion m ()
    ChangeFaction :: RegionId -> PlayerId -> UpdateRegion m ()

data PlayerMoveInputError =
    NotPlayerOwned RegionId |
    RegionDontExist RegionId |
    MoveTooMuch RegionId | 
    TooMuchReinforcement 
    deriving (Show, Generic, Eq)

instance FromJSON PlayerMoveInputError
instance ToJSON PlayerMoveInputError
instance Exception PlayerMoveInputError
instance ErrStatus (PlayerMoveInputError) where
    toErrStatus _ = internalServerError500

makeSem ''ReadMapInfo
makeSem ''UpdateRegion
makeSem ''CurrentPlayerInfo

newtype KeyNotFoundError k = KeyNotFoundError k deriving (Generic)
instance FromJSON a => FromJSON (KeyNotFoundError a) 
instance ToJSON a => ToJSON (KeyNotFoundError a)
instance ErrStatus (KeyNotFoundError k) where
    toErrStatus _ = notFound404 

runReadMapInfo :: Members '[State Game, Error PlayerMoveInputError] r => Sem (ReadMapInfo ': r) a -> Sem r a
runReadMapInfo = interpret $ \(GetRegionInfo regionId) -> do
    lookupResult <- gets (Data.Map.lookup regionId . view (turnInfo.gameMap))
    case lookupResult of
        Just region -> return region
        Nothing -> throw $ RegionDontExist regionId


runUpdateRegion:: Members '[State Game] r => Sem (UpdateRegion ': r) a -> Sem r a
runUpdateRegion = interpret $ \case
    (UpdatePopulation regionId newPops) ->
        modify (over (turnInfo.gameMap) (adjust (over population ((+) newPops)) regionId))
    (ChangeFaction regionId newFac) -> 
        modify (over (turnInfo.gameMap) (adjust (set faction (Just newFac)) regionId))

runCurrentPlayerInfo :: Members '[State Game, Reader PlayerId] r => Sem (CurrentPlayerInfo ': r) a -> Sem r a
runCurrentPlayerInfo = interpret $ \case
    GetCurrentPlayerId -> ask @PlayerId
    GetMaxReinforcement -> do
        playerId <- ask @PlayerId
        fromMaybe 0 <$> gets (Data.Map.lookup playerId . view (turnInfo.maxReinforcement))

 
lookupReaderMap :: Ord k => Members '[Reader (Map k a), Error (KeyNotFoundError k)] r => k -> Sem r a
lookupReaderMap key = do
    result <- asks (Data.Map.lookup key)
    case result of
        Just r  -> pure r
        Nothing -> throw (KeyNotFoundError key)

runGameState :: Members '[Lift STM] r => Sem (State Game ': r) a -> Sem (Reader (TVar Game) ': r) a
runGameState = reinterpret $ \case
    Get     -> asks readTVar >>= sendM
    Put s   -> do
        var <- ask
        sendM $ modifyTVar var (const s) 

runTVarGame :: Members '[Input GameId, Reader GameHub,  Error (KeyNotFoundError GameId)] r => Sem (Reader (TVar Game) ': r) a -> Sem r a
runTVarGame sem = do
    key <- input @GameId
    game <- lookupReaderMap key
    runReader game sem

runGameTurn :: 
    Members '[
        Reader PlayerId,
        Reader GameHub, 
        Input GameId, 
        Error (KeyNotFoundError GameId),
        Lift STM
    ] r => 
    Sem (
        State Game ':
        r
     ) a ->
    Sem r a
runGameTurn = 
    runTVarGame . 
    runGameState

runLogicPure ::
    Members '[ 
        State Game,
        Reader PlayerId,
        Error PlayerMoveInputError
    ] r => 
    Sem (
        UpdateRegion ':
        ReadMapInfo ':
        CurrentPlayerInfo ':
        r
     ) a ->
    Sem r a
runLogicPure = 
    runCurrentPlayerInfo .
    runReadMapInfo . 
    runUpdateRegion