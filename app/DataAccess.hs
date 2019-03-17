{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

type GameHub = Map GameId Game

newtype GameId = GameId Integer deriving (Num, Eq, Ord, Show, FromHttpApiData, Generic, ToJSON)

data Game = Game { gameBorders :: Borders, gameMap :: GameMap, reinforcement :: Map PlayerId Int}

newtype RiskyT a = RiskyT {runRiskyT :: ReaderT (TVar GameHub) IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (TVar GameHub))

class (Monad m) => AccessMapInfo m where
    getRegionInfo :: RegionId -> m RegionInfo

getGameHub :: RiskyT GameHub
getGameHub = asks readTVarIO >>= liftIO

getGame :: GameId -> RiskyT Game
getGame gameId = fmap (fromJust . Data.Map.lookup gameId) getGameHub
        
updateGame :: GameId -> GameStateMonad [UpdateRegionCommand] -> RiskyT GameMap
updateGame gameId f = do
    gameHubTVar <- ask
    liftIO $ atomically $ do
        game <- gameMap <$> readTVarGame gameHubTVar gameId
        case runExcept $ runReaderT f game of
            Right acts -> do
                let game' = Data.List.foldr updateDataRegion game acts
                modifyTVarGame gameHubTVar gameId (\(Game b _ r) -> Game b game' r)
                return game'
            Left err -> undefined  
modifyTVarGame :: TVar GameHub -> GameId -> (Game -> Game) -> STM ()
modifyTVarGame gameHub gameId f = modifyTVar gameHub (adjust f gameId)

readTVarGame :: TVar GameHub -> GameId -> STM Game
readTVarGame gameHub gameId = (fromJust . Data.Map.lookup gameId) <$> readTVar gameHub

updateDataRegion (UpdateRegionPopulation regionId newPops) (GameMap m) = GameMap $ adjust (\(RegionInfo f _) -> RegionInfo f newPops) regionId m
updateDataRegion (ChangeRegionFaction regionId newFac newPops) (GameMap m) = GameMap $ adjust (const (RegionInfo (Just newFac) newPops)) regionId m