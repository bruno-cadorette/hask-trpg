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
import Servant.JS
import Servant.JS.Vanilla
import Servant.HTML.Lucid
import Servant
import Servant.Server.StaticFiles
import Network.Wai.Handler.Warp

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
import Page.Game
import Lucid.Base
import DataAccess
import Routes
import PlayerManagement
import Data.Foldable

getGameIds :: RiskyT [GameId]
getGameIds = ask >>= liftIO . fmap keys . readTVarIO

getGameState :: GameId -> RiskyT GameMap
getGameState = fmap gameMap . getGame

mapBorders = fmap gameBorders . getGame

updateGameMap :: GameId -> PlayerId -> [GameAction] -> RiskyT GameMap
updateGameMap gameId _ act = do
    updateGame gameId $ fmap concat $ traverse handleMove act

gameApi gameId = (getGameState gameId :<|> updateGameMap gameId) :<|> mapBorders gameId

riskyApi :: ServerT FullApi RiskyT
riskyApi = (pure game) :<|> serveStaticFiles :<|> getGameIds :<|> gameApi  

riskyServer region = hoistServer (Proxy :: Proxy FullApi) (riskyTToHandler region) riskyApi

writeJSFiles :: IO ()
writeJSFiles = writeJSForAPI (Proxy :: Proxy GameApi) vanillaJS "/home/bruno/git/risky/app/static/api.js"

serveStaticFiles :: ServerT FileApi RiskyT
serveStaticFiles = serveDirectoryWebApp "/home/bruno/git/risky/app/static"

riskyTToHandler :: TVar GameHub -> RiskyT a -> Handler a
riskyTToHandler newRegion r = liftIO $ runReaderT (runRiskyT r) newRegion

app region = serve (Proxy :: Proxy FullApi) (riskyServer region)

initGame = Game (borders 15 15) (baseRegions 15 15) (fromList [(PlayerId 1, 2), (PlayerId 2, 2)])

main :: IO ()
main = do
    newGame <- newTVarIO $ Data.Map.fromList [(1, initGame)]
    writeJSFiles
    run 8081 (app newGame)

