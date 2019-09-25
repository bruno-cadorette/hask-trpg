{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}


module Routes where

import Servant
import Servant.API
import Servant.HTML.Lucid
import Lucid.Base
import Game.Effects
import Game.Logic
import Region
import Servant.Checked.Exceptions
import PlayerManagement
import Servant.Foreign
import Servant.Auth.Server

type FileApi = "static" :> Raw

type FullApi = Website :<|> FileApi :<|> GameApi
type Website = Get '[HTML] (Html ()) -- :<|> 
   {- "game" :>
        Capture "gameId" GameId :>
            Get '[JSON] GameMap
    -}

instance HasForeign lang ftype api
  => HasForeign lang ftype ((Throws a) :> api) where
  type Foreign ftype ((Throws a) :> api) = Foreign ftype api

  foreignFor lang ftype Proxy req =
    foreignFor lang ftype (Proxy :: Proxy api) req

type PlayerAuthentication = Auth '[JWT] PlayerId 

type GameManagement =
    PlayerAuthentication :> ReqBody '[JSON] GameId :> Post '[JSON] Bool

type FullApi' = 
    PlayerAuthentication :>
        "game" :> GameApi'
        
type GameApi' =
    Get '[JSON] [GameId] :<|>
    Capture "gameId" GameId :> Throws (KeyNotFoundError GameId) :> (
        "gameState" :> (
            Get '[JSON] GameMap :<|>
            ReqBody '[JSON] [Move] :> Throws PlayerMoveInputError :> Post '[JSON] ()
        ) :<|>
        "borders" :> Get '[JSON] Borders
    )
        

type GameApi = 
    "game" :> (
        Get '[JSON] [GameId] :<|>
        Capture "gameId" GameId :> Throws (KeyNotFoundError GameId) :> (
            "gameState" :> (
                Get '[JSON] GameMap :<|>
                Capture "playerId" PlayerId :> ReqBody '[JSON] [Move] :> Throws PlayerMoveInputError :> Post '[JSON] ()
            ) :<|>
            "borders" :> Get '[JSON] Borders
        )
    )