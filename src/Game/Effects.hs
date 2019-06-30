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

data TurnInfo = TurnInfo { _gameMap :: GameMap, _reinforcement :: Map PlayerId Int, _turnNumber :: Int }

makeLenses ''TurnInfo

data Game = Game { _gameBorders :: Borders, _turnInfo :: TurnInfo}

makeLenses ''Game

type GameHub = Map GameId (TVar Game)

newtype GameId = GameId Integer deriving (Num, Eq, Ord, Show, FromHttpApiData, Generic, ToJSON)

data ReadMapInfo m a where
    GetRegionInfo :: RegionId -> ReadMapInfo m RegionInfo

data UpdateRegion m a where
    UpdatePopulation :: RegionId -> Army -> UpdateRegion m ()
    ChangeFaction :: RegionId -> PlayerId -> Army -> UpdateRegion m ()

data PlayerMoveInputError =
    SelectedEmptySource RegionId |
    RegionDontExist RegionId |
    MoveTooMuch RegionId Army Army
    deriving (Show, Generic)

instance FromJSON PlayerMoveInputError
instance ToJSON PlayerMoveInputError
instance Exception PlayerMoveInputError
instance ErrStatus (PlayerMoveInputError) where
    toErrStatus _ = internalServerError500

makeSem ''ReadMapInfo
makeSem ''UpdateRegion

newtype KeyNotFoundError k = KeyNotFoundError k deriving (Generic)
instance FromJSON a => FromJSON (KeyNotFoundError a) 
instance ToJSON a => ToJSON (KeyNotFoundError a)
instance ErrStatus (KeyNotFoundError k) where
    toErrStatus _ = notFound404 

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

 
lookupReaderMap :: Ord k => Members '[Reader (Map k a), Error (KeyNotFoundError k)] r => k -> Sem r a
lookupReaderMap key = do
    result <- asks (Data.Map.lookup key)
    case result of
        Just r  -> pure r
        Nothing -> throw (KeyNotFoundError key)


runGameMapState :: Members '[Lift STM] r => Sem (State GameMap ': r) a -> Sem (Reader (TVar Game) ': r) a
runGameMapState = reinterpret $ \case
    Get   -> do
        var <- ask
        sendM $ fmap (view (turnInfo.gameMap)) $ readTVar var
    Put s -> do
        var <- ask
        sendM $ modifyTVar var (set (turnInfo.gameMap) s)   

runTVarGame :: Members '[Input GameId, Reader GameHub,  Error (KeyNotFoundError GameId)] r => Sem (Reader (TVar Game) ': r) a -> Sem r a
runTVarGame sem = do
    key <- input
    game <- lookupReaderMap key
    runReader game sem

runGameTurn :: 
    Members '[
        Reader GameHub, 
        Input GameId, 
        Error (KeyNotFoundError GameId),
        Error PlayerMoveInputError,
        Lift STM
    ] r => 
    Sem (
        UpdateRegion ':
        ReadMapInfo ':
        State GameMap ':
        r
     ) a ->
    Sem r a
runGameTurn = 
    runTVarGame . 
    runGameMapState .
    runReadMapInfo . 
    runUpdateRegion