{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Servant.API

import Data.List
import Data.Maybe
import GHC.Generics
import Servant
import Servant.Server.StaticFiles
import Network.Wai.Handler.Warp
import Data.Map 
import Data.Aeson
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Region
import Servant.JS
import Servant.JS.Vanilla
import GameState

newtype GameId = GameId Integer deriving (Num, Eq, Ord, Show, FromHttpApiData)

data Game = Game { gameBorders :: Borders, gameMap :: GameMap}

type GameHub = Map GameId Game

type FileApi = "static" :> Raw

type FullApi = FileApi :<|> GameApi


type GameApi = 
    "game" :> Capture "gameId" GameId :> (
        "gameState" :> (
            Get '[JSON] GameMap :<|>
            ReqBody '[JSON] [GameAction] :> Post '[JSON] GameMap
        ) :<|>
        "borders" :> Get '[JSON] Borders
    )

newtype RiskyT a = RiskyT {runRiskyT :: ReaderT (TVar GameHub) IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (TVar GameHub))

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


getGameState :: GameId -> RiskyT GameMap
getGameState = fmap gameMap . getGame

mapBorders = fmap gameBorders . getGame

gameApi :: ServerT GameApi RiskyT
gameApi gameId = (getGameState gameId :<|> updateGameState gameId) :<|> mapBorders gameId

riskyApi :: ServerT FullApi RiskyT
riskyApi = serveStaticFiles :<|> gameApi  

riskyServer region = hoistServer (Proxy :: Proxy FullApi) (riskyTToHandler region) riskyApi

writeJSFiles :: IO ()
writeJSFiles = writeJSForAPI (Proxy :: Proxy GameApi) vanillaJS "/home/bruno/git/risky/app/static/api.js"

serveStaticFiles :: ServerT FileApi RiskyT
serveStaticFiles = serveDirectoryWebApp "/home/bruno/git/risky/app/static"

riskyTToHandler :: TVar GameHub -> RiskyT a -> Handler a
riskyTToHandler newRegion r = liftIO $ runReaderT (runRiskyT r) newRegion

app region = serve (Proxy :: Proxy FullApi) (riskyServer region)


initGame = Game (borders 15 15) (baseRegions 15 15)

main :: IO ()
main = do
    newGame <- newTVarIO $ Data.Map.fromList [(1, initGame)]
    writeJSFiles
    run 8081 (app newGame)

