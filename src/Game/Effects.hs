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
import qualified Control.Monad.State.Lazy as Mtl

data Game = Game { _gameBorders :: Borders, _turnNumber :: Int, _unitPositions :: UnitPositions}

makeLenses ''Game

type GameHub = Map GameId (TVar Game)

newtype GameId = GameId Integer deriving (Num, Eq, Ord, Show, FromHttpApiData, Generic, ToJSON)

data CurrentPlayerInfo m a where
    GetCurrentPlayerId :: CurrentPlayerInfo m PlayerId

data PlayerMoveInputError =
    NotPlayerOwned RegionId |
    RegionOccupied RegionId |
    RegionNotOccupied RegionId |
    AttackAllies RegionId RegionId | 
    AttackTooFar RegionId RegionId |
    MoveTooMuch RegionId 
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


runCurrentPlayerInfo :: Members '[Reader PlayerId] r => InterpreterFor CurrentPlayerInfo r
runCurrentPlayerInfo = interpret $ \case
    GetCurrentPlayerId -> ask @PlayerId

 
lookupReaderMap :: Ord k => Members '[Reader (Map k a), Error (KeyNotFoundError k)] r => k -> Sem r a
lookupReaderMap key = do
    result <- asks (Data.Map.lookup key)
    case result of
        Just r  -> pure r
        Nothing -> throw (KeyNotFoundError key)

runStateAsReaderTVar :: Members '[Embed STM] r => Sem (State s ': r) a -> Sem (Reader (TVar s) ': r) a
runStateAsReaderTVar = reinterpret $ \case
    Get     -> asks readTVar >>= embed
    Put s   -> do
        var <- ask
        embed $ modifyTVar var (const s) 

runTVarGame :: Members '[Input GameId, Reader GameHub,  Error (KeyNotFoundError GameId)] r => InterpreterFor (Reader (TVar Game)) r
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
        Error PlayerMoveInputError,
        Embed STM
    ] r => 
    Sem (
        ReadMapInfo ': 
        UnitAction ': 
        State UnitPositions ':
        State Game ':
        CurrentPlayerInfo ':
        r
     ) a ->
    Sem r a
runGameTurn = runCurrentPlayerInfo . runTVarGame . runStateAsReaderTVar . runPlayerActions


runPlayerActions :: Member (State Game) r => Sem (ReadMapInfo ': UnitAction ': State UnitPositions ': r) a -> Sem r a
runPlayerActions = zoomEffect unitPositions . runUnitMoving . runReadMapInfo

    
zoomEffect :: Member (State outer) r => Lens' outer inner -> InterpreterFor (State inner) r
zoomEffect lens stateSem = do
    innerState <- gets (view lens)
    Mtl.evalStateT (hoistStateIntoStateT stateSem) innerState 