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

data GameAction = Move {origin :: RegionId, destination :: RegionId, troops :: Integer} deriving(Generic, Show)

instance FromJSON GameAction
instance ToJSON GameAction

adjustGet :: Ord k => (a -> a) -> k -> Map k a -> (Maybe a, Map k a)
adjustGet f k m = 
    case Data.Map.lookup k m of
        Just a  -> (Just a, Data.Map.insert k (f a) m)
        Nothing -> (Nothing, m)

move (Move originId destinationId troopsNumber) m = 
    let (x, m') = adjustGet (\(RegionInfo f p) -> RegionInfo f (p - troopsNumber)) originId m
    in maybe m' (\(RegionInfo f1 _) -> adjust (invasion f1 troopsNumber) destinationId m') x
    
invasion attackerFaction attackerTroops (RegionInfo defenderFaction defenderTroops)
    | attackerFaction == defenderFaction =
        RegionInfo defenderFaction (attackerTroops + defenderTroops)
    | defenderTroops - attackerTroops < 0 =
        RegionInfo attackerFaction (attackerTroops - defenderTroops)
    | otherwise =
        RegionInfo defenderFaction (defenderTroops - attackerTroops)

reinforce = Data.Map.map (\(RegionInfo f p) -> RegionInfo f (if f == FactionId 0 then p else p + 1))



type FileApi = "static" :> Raw

type MapApi = "borders" :> Get '[JSON] (Map RegionId [RegionId])

type RiskyApi = GameStateApi :<|> MapApi

type FullApi = FileApi :<|> RiskyApi

type GameStateApi = "gameState" :> (Get '[JSON] GameMap :<|> (ReqBody '[JSON] [GameAction] :> Post '[JSON] GameMap))

type RiskyT = ReaderT (TVar GameMap) IO

updateGameState :: [GameAction] -> RiskyT GameMap
updateGameState act = do
    gameMap <- ask
    m' <- lift $ atomically $ do
        modifyTVar gameMap (\m -> reinforce $ Data.List.foldr move m act) 
        readTVar gameMap
    liftIO $ print (Data.Map.lookup "2_2" m', Data.Map.lookup "2_3" m')
    return m'


getGameState :: RiskyT GameMap
getGameState = asks readTVarIO >>= lift

gameStateApi :: ServerT GameStateApi RiskyT
gameStateApi = getGameState :<|> updateGameState


mapApi :: ServerT MapApi RiskyT
mapApi = return (borders 15 15)

riskyApi :: ServerT FullApi RiskyT
riskyApi = serveStaticFiles :<|> gameStateApi :<|> mapApi  

riskyServer region = hoistServer (Proxy :: Proxy FullApi) (riskyTToHandler region) riskyApi

writeJSFiles :: IO ()
writeJSFiles = writeJSForAPI (Proxy :: Proxy RiskyApi) vanillaJS "/home/bruno/git/risky/app/static/api.js"

serveStaticFiles :: ServerT FileApi RiskyT
serveStaticFiles = serveDirectoryWebApp "/home/bruno/git/risky/app/static"

riskyTToHandler newRegion r = liftIO $ runReaderT r newRegion

app region = serve (Proxy :: Proxy FullApi) (riskyServer region)

main :: IO ()
main = do
    newRegion <- newTVarIO $ baseRegions 15 15
    writeJSFiles
    run 8081 (app newRegion)

