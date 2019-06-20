{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, BlockArguments, TypeApplications, TypeFamilies #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds, RankNTypes, ScopedTypeVariables #-}

module DataAccess where
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
import Polysemy.Reader
import Polysemy.State

data TurnInfo = TurnInfo { _gameMap :: GameMap, _reinforcement :: Map PlayerId Int, _turnNumber :: Int }

makeLenses ''TurnInfo

data Game = Game { _gameBorders :: Borders, _turnInfo :: TurnInfo}

makeLenses ''Game

type GameHub = Map GameId Game

newtype GameId = GameId Integer deriving (Num, Eq, Ord, Show, FromHttpApiData, Generic, ToJSON)


--newtype RiskyT a = RiskyT {runRiskyT :: ReaderT (TVar GameHub) IO a }
--    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (TVar GameHub))


modifyTVarGame :: TVar GameHub -> GameId -> (Game -> Game) -> STM ()
modifyTVarGame gameHub gameId f = modifyTVar gameHub (adjust f gameId)


--getGameHub :: RiskyT GameHub
--getGameHub = asks readTVarIO >>= liftIO

--getGame' :: GameId -> RiskyT Game
--getGame' gameId = fmap (fromJust . Data.Map.lookup gameId) getGameHub

readTVarGame :: TVar GameHub -> GameId -> STM Game
readTVarGame gameHub gameId = (fromJust . Data.Map.lookup gameId) <$> readTVar gameHub
    


data ReadGameManagement m a where
    GetGame :: GameId -> ReadGameManagement m Game

makeSem ''ReadGameManagement
{-
updateGame' :: (Members [UpdateRegion, Error PlayerMoveInputError, ReadMapInfo, PlayerInformation] r) => GameId -> [Move] -> Sem r ()
updateGame' gameId playerInputs = do
    game <- getGame gameId
    traverse_ handleMove playerInputs

--Member (Reader (TVar Game))

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





ReaderT (TVar GameHub) IO a
-}

runReadMapInfo :: Members '[State GameMap] r => Sem (ReadMapInfo ': r) a -> Sem r a
runReadMapInfo = interpret $ \(GetRegionInfo regionId) -> do
    lookupResult <- gets (Data.Map.lookup regionId)
    case lookupResult of
        Just gameMap -> undefined
        Nothing -> throw $ RegionDontExist regionId


--TVar GameMap -> State GameMap ???
runUpdateRegion :: Members '[Reader (TVar GameMap), Lift STM] r => Sem (UpdateRegion ': r) a -> Sem r a
runUpdateRegion = interpret $ \case
    (UpdatePopulation regionId newPops) ->  do
        gameMapTVar <- ask
        sendM (modifyTVar gameMapTVar (\(GameMap m) -> GameMap $ adjust (set population newPops) regionId m))
    (ChangeFaction regionId newFac newPops) -> do
        gameMapTVar <- ask
        sendM (modifyTVar gameMapTVar (\(GameMap m) -> GameMap $ Data.Map.insert regionId (RegionInfo (Just newFac) newPops)  m))
        