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
{-# LANGUAGE TemplateHaskell #-}


module Routes where

import Servant
import Servant.API
import Servant.HTML.Lucid
import Lucid.Base
import Game.Effects
import Character.Stats
import Game.Logic
import Region
import TileMap.Environment
import Soldier
import Servant.Checked.Exceptions
import PlayerManagement
import Servant.Foreign
--import Servant.Elm
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

--type FullApi' = 
--    PlayerAuthentication :>
--        "game" :> GameApi'


type ActionApi = 
    ReqBody '[JSON] Position :> (
        Get '[JSON] [Action] :<|> 
        ReqBody '[JSON] ActionId :> (
            Get '[JSON] [Position] :<|>
            ReqBody '[JSON] Position :>
                Post '[JSON] ()
        )
    )
type SingleGameApi = 
    Capture "playerId" PlayerId :> 
        Capture "gameId" GameId :> (
            ActionApi :<|>
                "borders" :> Get '[JSON] Borders
            )

type GameApi = 
    "game" :> (
        Get '[JSON] [GameId] :<|>
        SingleGameApi
    )

{-
deriveElmDef defaultOptions ''GameId
deriveElmDef defaultOptions ''Position
deriveElmDef defaultOptions ''CharacterUnit
deriveElmDef defaultOptions ''KeyNotFoundError
deriveElmDef defaultOptions ''PlayerId
deriveElmDef defaultOptions ''PlayerInput
deriveElmDef defaultOptions ''PlayerInputType
deriveElmDef defaultOptions ''PlayerMoveInputError
deriveElmDef defaultOptions ''UnitPositions
deriveElmDef defaultOptions ''TerrainType
deriveElmDef defaultOptions ''Borders

elmTypes = [
    DefineElm (Proxy :: Proxy GameId), 
    DefineElm (Proxy :: Proxy Position), 
    DefineElm (Proxy :: Proxy CharacterUnit), 
    DefineElm (Proxy :: Proxy (KeyNotFoundError a)), 
    DefineElm (Proxy :: Proxy PlayerId),
    DefineElm (Proxy :: Proxy PlayerInput),
    DefineElm (Proxy :: Proxy PlayerInputType),
    DefineElm (Proxy :: Proxy PlayerMoveInputError),
    DefineElm (Proxy :: Proxy UnitPositions),
    DefineElm (Proxy :: Proxy TerrainType),
    DefineElm (Proxy :: Proxy Borders)]

elm = 
    generateElmModule
        ["GameApi"]
        defElmImports
        "GameApi"
        elmTypes
        (Proxy :: Proxy GameApi)
-}