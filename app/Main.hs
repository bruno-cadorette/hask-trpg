



{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Main where

import Servant.API
import Servant.JS
import Servant.JS.Vanilla
import Servant.HTML.Lucid
import Servant
import Servant.Server.StaticFiles
import Network.Wai.Handler.Warp
import Control.Lens
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
import Control.Monad.IO.Class
import Servant.Checked.Exceptions
import Data.Either
import Control.Monad
import Servant.Auth.Server
import ServerHandler

instance ToJWT PlayerId
instance FromJWT PlayerId


type GameMonad = Sem '[Reader GameHub, Embed STM]

runErrors :: 
    IsMember (KeyNotFoundError (GameId)) es => 
    IsMember PlayerMoveInputError es => 
    Sem (Error PlayerMoveInputError ': Error (KeyNotFoundError (GameId)) ': r) a -> 
    Sem r (Envelope es a)
runErrors = fmap join . runErrorToEnv @(KeyNotFoundError GameId) . runErrorToEnv @PlayerMoveInputError


getGame :: Members '[Reader GameHub, Embed STM, Error (KeyNotFoundError GameId)] r => GameId -> Sem r Game
getGame gameId = do 
    game <- lookupReaderMap gameId
    embed $ readTVar game

getGameIds :: GameMonad [GameId]
getGameIds = asks keys

getGameState :: GameId -> GameMonad (Envelope '[KeyNotFoundError GameId] UnitPositions)
getGameState = runErrorToEnv . fmap (view $ unitPositions) . getGame


mapBorders :: GameId -> GameMonad (Envelope '[KeyNotFoundError GameId] Borders)
mapBorders = runErrorToEnv . fmap (_gameBorders) . getGame

updateGameMap :: GameId -> PlayerId -> [PlayerInput] -> GameMonad (Envelope '[KeyNotFoundError GameId, PlayerMoveInputError] ())
updateGameMap gameId playerId moves = 
    runErrors $ runReader playerId $ runInputConst gameId $ runGameTurn $ traverse_ handlePlayerInput moves

gameApi gameId = (getGameState gameId :<|> updateGameMap gameId) :<|> mapBorders gameId


riskyApi :: ServerT FullApi GameMonad
riskyApi = (pure game) :<|> serveStaticFiles :<|> getGameIds :<|> gameApi  

riskyServer region = hoistServer (Proxy :: Proxy FullApi) (riskyTToHandler region) riskyApi

writeJSFiles :: IO ()
writeJSFiles = writeJSForAPI (Proxy :: Proxy GameApi) vanillaJS "/home/bruno/git/risky/app/static/api.js"



serveStaticFiles :: ServerT FileApi GameMonad
serveStaticFiles = serveDirectoryWebApp "/home/bruno/git/risky/app/static"


initGame = Game (borders 15 15) 0 baseUnitPositions




riskyTToHandler :: GameHub -> GameMonad a -> Handler a
riskyTToHandler hub r = liftIO $ atomically $ runM $ runReader hub r


app region = serve (Proxy :: Proxy FullApi) (riskyServer region)

main :: IO ()
main = do
    game <- newTVarIO initGame
    jwtConfig <- fmap defaultJWTSettings generateKey
    let newGame = Data.Map.fromList [(1, game)]
    writeJSFiles
    Network.Wai.Handler.Warp.run 8081 (app newGame)