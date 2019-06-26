{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}

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
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Region
import Game.Effects
import Page.Game
import Lucid.Base
import Game.Logic
import Routes
import PlayerManagement
import Data.Foldable
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Reader

getGameIds :: RiskyT [GameId]
getGameIds = undefined-- ask >>= liftIO . fmap keys . readTVarIO

getGameState = undefined--fmap gameMap . getGame

runRiskyT = undefined

mapBorders = undefined--fmap gameBorders . getGameIds

updateGameMap :: Members '[Reader GameHub, Input GameId, Input PlayerId, Error GameHubError] r => Sem r a
updateGameMap gameId pid hub act = do
    updateGame pid (hub Data.Map.! gameId) act

actionOrders :: [(PlayerId, [GameAction])] -> [(PlayerId, GameAction)]
actionOrders = concat . transpose . fmap (\(pid, as) -> fmap (pid,) as)


gameApi gameId = (getGameState gameId :<|> updateGameMap gameId) :<|> mapBorders gameId

riskyApi :: ServerT FullApi RiskyT
riskyApi = undefined--(pure game) :<|> serveStaticFiles :<|> getGameIds :<|> gameApi  

riskyServer region = hoistServer (Proxy :: Proxy FullApi) (riskyTToHandler region) riskyApi

writeJSFiles :: IO ()
writeJSFiles = writeJSForAPI (Proxy :: Proxy GameApi) vanillaJS "/home/bruno/git/risky/app/static/api.js"

serveStaticFiles :: ServerT FileApi RiskyT
serveStaticFiles = serveDirectoryWebApp "/home/bruno/git/risky/app/static"

riskyTToHandler :: TVar GameHub -> RiskyT a -> Handler a
riskyTToHandler newRegion r = liftIO $ runReaderT (runRiskyT r) newRegion

app region = serve (Proxy :: Proxy FullApi) (riskyServer region)

initGame = undefined--Game (borders 15 15) (baseRegions 15 15) (fromList [(PlayerId 1, 2), (PlayerId 2, 2)])

main :: IO ()
main = do
    newGame <- newTVarIO $ Data.Map.fromList [(1, initGame)]
    writeJSFiles
    Network.Wai.Handler.Warp.run 8081 (app newGame)

