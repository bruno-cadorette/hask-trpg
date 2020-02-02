



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
import Game.Logic
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
import Polysemy.State
import Polysemy.Reader
import Control.Monad.IO.Class
import Servant.Checked.Exceptions
import Data.Either
import Character.Stats
import Control.Monad
import Servant.Auth.Server
import ServerHandler
import Debug.Trace
import Ai
import Data.Coerce
import TileMap.Environment
import Data.Bitraversable

instance ToJWT PlayerId
instance FromJWT PlayerId


type GameMonad = Sem '[Reader (TVar Game), Embed STM]

runErrors :: 
    Sem (Error PlayerMoveInputError ': r) a -> 
    Sem r (Envelope '[PlayerMoveInputError] a)
runErrors = runErrorToEnv @PlayerMoveInputError

ignoreErrors ::
    Monoid a =>
    Sem (Error PlayerMoveInputError ': r) a -> 
    Sem r (Envelope '[PlayerMoveInputError] a)
ignoreErrors = fmap (const (SuccEnvelope mempty)). runErrors

getGame :: Members '[Reader (TVar Game), Embed STM] r => Sem r Game
getGame = do 
    game <- ask
    embed $ readTVar game

getGameIds :: GameMonad [GameId]
getGameIds = pure [GameId 1]

getGameState :: GameMonad UnitPositions
getGameState = fmap (view unitPositions) $ getGame


mapBorders :: GameMonad Borders
mapBorders = fmap (view $ gameBorders) $ getGame
{-
updateAi = runStateAsReaderTVar $ runErrors $ runPlayerActions $ do
    game <- gets (view unitPositions)
    case generateMove (PlayerId 2) (coerce game) of 
        Just g -> 
            runReader @PlayerId (PlayerId 2) $ runCurrentPlayerInfo $ handlePlayerInput g
        Nothing -> pure ()

updateGameMap :: PlayerId -> PlayerInput -> GameMonad (Envelope '[PlayerMoveInputError] ())
updateGameMap playerId moves = 
    runStateAsReaderTVar $ runErrors $ runPlayerActions $ runReader @PlayerId playerId $ runCurrentPlayerInfo $ handlePlayerInput moves
-}


gameApi :: Members '[ReadMapInfo, Error PlayerMoveInputError, UnitAction] r => ServerT SingleGameApi (Sem r)
gameApi playerId gameId = hoistServer (Proxy :: Proxy ActionApi) (runCurrentPlayerInfo playerId) actionHandlers :<|> getAllTiles


--riskyApi :: ServerT FullApi GameMonad
--riskyApi = (pure game) :<|> serveStaticFiles :<|> getGameIds :<|> gameApi  

riskyServer game = hoistServer (Proxy :: Proxy SingleGameApi) (gameMonadToHandler game) gameApi

writeJSFiles :: IO ()
writeJSFiles = writeJSForAPI (Proxy :: Proxy GameApi) vanillaJS "/home/bruno/git/risky/app/static/api.js"



serveStaticFiles :: ServerT FileApi GameMonad
serveStaticFiles = serveDirectoryWebApp "/home/bruno/git/risky/app/static"


initGame = Game (borders 15 15) 0 baseUnitPositions

unsafeRunError = fmap (fromRight undefined) . runError

gameMonadToHandler :: TVar Game -> Sem '[ReadMapInfo, UnitAction, Error PlayerMoveInputError, State Game, Embed STM] a -> Handler a
gameMonadToHandler game r = liftIO $ atomically  $ runM $ runReader game $ runStateAsReaderTVar $ unsafeRunError $ runPlayerActions r


actionHandlers :: Members '[CurrentPlayerInfo, ReadMapInfo, Error PlayerMoveInputError, UnitAction] r => ServerT ActionApi (Sem r)
actionHandlers = \p -> (getPossibleActions p :<|> (\a -> (getActionRange p a :<|> handlePlayerInput' p a)))


app region = serve (Proxy :: Proxy SingleGameApi) (riskyServer region)

main :: IO ()
main = do
    putStrLn "Start server"
    game <- newTVarIO initGame
    jwtConfig <- fmap defaultJWTSettings generateKey
    Network.Wai.Handler.Warp.run 8081 (app game)