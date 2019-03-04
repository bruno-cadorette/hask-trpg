{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GameStateManagement where

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

type GameHub = Map GameId Game

newtype GameId = GameId Integer deriving (Num, Eq, Ord, Show, FromHttpApiData, ToJSON)

data Game = Game { gameBorders :: Borders, gameMap :: GameMap}

newtype RiskyT a = RiskyT {runRiskyT :: ReaderT (TVar GameHub) IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (TVar GameHub))

getGameHub :: RiskyT GameHub
getGameHub = asks readTVarIO >>= liftIO

getGame :: GameId -> RiskyT Game
getGame gameId = asks (fmap (fromJust . Data.Map.lookup gameId) . readTVarIO) >>= liftIO

updateGameState :: GameId -> [GameAction] -> RiskyT GameMap
updateGameState gameId act = do
    gameHub <- ask
    m' <- liftIO $ atomically $ do
        modifyTVarGame gameHub gameId (\m -> m { gameMap = reinforce $ Data.List.foldr move (gameMap m) act }) 
        gameMap <$> readTVarGame gameHub gameId
        
    return $ m'

modifyTVarGame :: TVar GameHub -> GameId -> (Game -> Game) -> STM ()
modifyTVarGame gameHub gameId f = modifyTVar gameHub (adjust f gameId)

readTVarGame :: TVar GameHub -> GameId -> STM Game
readTVarGame gameHub gameId = (fromJust . Data.Map.lookup gameId) <$> readTVar gameHub
