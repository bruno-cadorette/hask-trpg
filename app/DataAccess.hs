{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds #-}

module DataAccess where
import Servant
import Data.List
import Data.Maybe
import GHC.Generics
import Data.Map 
import Data.Aeson
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Region
import GameState
import PlayerManagement
import Control.Monad.Except
import Control.Lens
import Polysemy


data TurnInfo = TurnInfo { _gameMap :: GameMap, _reinforcement :: Map PlayerId Int, _turnNumber :: Int }

makeLenses ''TurnInfo

data Game = Game { _gameBorders :: Borders, _turnInfo :: TurnInfo}

makeLenses ''Game

type GameHub = Map GameId Game

newtype GameId = GameId Integer deriving (Num, Eq, Ord, Show, FromHttpApiData, Generic, ToJSON)


newtype RiskyT a = RiskyT {runRiskyT :: ReaderT (TVar GameHub) IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (TVar GameHub))


modifyTVarGame :: TVar GameHub -> GameId -> (Game -> Game) -> STM ()
modifyTVarGame gameHub gameId f = modifyTVar gameHub (adjust f gameId)


getGameHub :: RiskyT GameHub
getGameHub = asks readTVarIO >>= liftIO

getGame' :: GameId -> RiskyT Game
getGame' gameId = fmap (fromJust . Data.Map.lookup gameId) getGameHub

readTVarGame :: TVar GameHub -> GameId -> STM Game
readTVarGame gameHub gameId = (fromJust . Data.Map.lookup gameId) <$> readTVar gameHub
    


data ReadGameManagement m a where
    GetGame :: GameId -> ReadGameManagement m Game

makeSem ''ReadGameManagement

updateGame' gameId playerInputs = do
    game <- getGame gameId
    traverse_ handleMove playerInputs

{-

updateGame gameId = do
    gameHubTVar <- ask
    liftIO $ atomically $ do
        game <- (view $ turnInfo.gameMap) <$> readTVarGame gameHubTVar gameId
        case runExcept $ runReaderT f game of
            Right acts -> do
                let game' = Data.List.foldr updateDataRegion game acts
                modifyTVarGame gameHubTVar gameId (
                    set (turnInfo.reinforcement) (fromList $ getNextReinforcement game') .
                    over (turnInfo.turnNumber) (+ 1) . 
                    set (turnInfo.gameMap) game')
            Left err -> undefined  

-}


runUpdateRegion :: GameMap -> Sem (UpdateRegion ': r) a -> Sem r GameMap
runUpdateRegion (GameMap m) = interpret $ \case
    UpdatePopulation regionId newPops -> GameMap $ adjust (\(RegionInfo f _) -> RegionInfo f newPops) regionId m
    ChangeFaction regionId newFac newPops -> GameMap $ adjust (const (RegionInfo (Just newFac) newPops)) regionId m