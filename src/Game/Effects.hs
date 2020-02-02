{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, BlockArguments, TypeApplications, TypeFamilies #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds, RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Game.Effects where

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
import Polysemy.State
import Polysemy.Reader
import Polysemy.Internal.Combinators
import Polysemy.Input
import Servant.Checked.Exceptions
import Network.HTTP.Types.Status
import Debug.Trace
import qualified Control.Monad.State.Lazy as Mtl
import TileMap.Environment


type GameHub = TVar (Map GameId (TVar Game))

newtype GameId = GameId Integer deriving (Num, Eq, Ord, Show, FromHttpApiData, Generic, ToJSON)

data CurrentPlayerInfo m a where
    GetCurrentPlayerId :: CurrentPlayerInfo m PlayerId

data PlayerMoveInputError =
    NotPlayerOwned Position |
    RegionOccupied Position |
    RegionNotOccupied Position |
    InvalidActionRange Position Position |
    MoveTooMuch Position |
    ExpectedEmpty Position |
    ExpectedEnemy Position
    deriving (Show, Generic, Eq)

instance FromJSON PlayerMoveInputError
instance ToJSON PlayerMoveInputError
instance Exception PlayerMoveInputError
instance ErrStatus PlayerMoveInputError where
    toErrStatus _ = internalServerError500

makeSem ''CurrentPlayerInfo

newtype KeyNotFoundError k = KeyNotFoundError k deriving (Generic)
instance FromJSON a => FromJSON (KeyNotFoundError a) 
instance ToJSON a => ToJSON (KeyNotFoundError a)
instance ErrStatus (KeyNotFoundError k) where
    toErrStatus _ = notFound404 


runCurrentPlayerInfo :: PlayerId -> InterpreterFor CurrentPlayerInfo r
runCurrentPlayerInfo playerId = interpret $ \case
    GetCurrentPlayerId -> return playerId

lookupGame :: Members '[Reader GameHub, Embed STM, Error (KeyNotFoundError GameId)] r => GameId -> Sem r (TVar Game)
lookupGame key = do
    gameHubTVar <- ask @GameHub
    gameHubMap <- embed $ readTVar $ gameHubTVar
    lookupMap key gameHubMap
    
lookupMap :: Ord k => Members '[Error (KeyNotFoundError k)] r => k -> Map k a -> Sem r a
lookupMap key map = do
    case Data.Map.lookup key map of
        Just r  -> pure r
        Nothing -> throw (KeyNotFoundError key)

runStateAsReaderTVar :: Show s => Members '[Embed STM] r => Sem (State s ': r) a -> Sem (Reader (TVar s) ': r) a
runStateAsReaderTVar = reinterpret $ \case
    Get     -> (trace "runStateAsReaderTVar" $ asks readTVar) >>= embed
    Put s   -> do
        var <- ask
        embed $ modifyTVar var (const s) 


runPlayerActions :: Member (State Game) r => Sem (ReadMapInfo ': UnitAction ': r) a -> Sem r a
runPlayerActions = runUnitMoving . runReadMapInfo

    
zoomEffect :: Member (State outer) r => Lens' outer inner -> InterpreterFor (State inner) r
zoomEffect lens stateSem = do
    innerState <- gets (view lens)
    Mtl.evalStateT (hoistStateIntoStateT stateSem) innerState 