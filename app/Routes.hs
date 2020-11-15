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
import Game.Logic
import Region
import Soldier
import Servant.Checked.Exceptions
import PlayerManagement
import Servant.Foreign
import Servant.Elm
import Servant.Auth.Server
import qualified Elm.Derive as Elm

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

type CaptureCoordinate = Capture "x" Int :> Capture "y" Int

type GameApi = 
  "unitpositions" :> Get '[JSON] UnitPositions :<|>
  ("playeraction" :> Capture "playerId" PlayerId :> ReqBody '[JSON] PlayerInput :> Post '[JSON] ()) :<|>
  ("possibleactions" :> Capture "playerId" PlayerId :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] [PossibleInput]) :<|>
  "borders" :> Get '[JSON] Borders
    
$(deriveBoth (Elm.defaultOptionsDropLower 1) ''SoldierUnit)
$(deriveBoth (Elm.defaultOptionsDropLower 1) ''GameId)
$(deriveBoth (Elm.defaultOptionsDropLower 1) ''Position)
$(deriveBoth (Elm.defaultOptionsDropLower 1) ''KeyNotFoundError)
$(deriveBoth (Elm.defaultOptionsDropLower 1) ''PlayerId)
$(deriveBoth (Elm.defaultOptionsDropLower 1) ''PlayerInput)
$(deriveBoth (Elm.defaultOptionsDropLower 1) ''PlayerInputType)
$(deriveBoth (Elm.defaultOptionsDropLower 1) ''PlayerMoveInputError)
$(deriveBoth (Elm.defaultOptionsDropLower 1) ''UnitPositions)
$(deriveBoth (Elm.defaultOptionsDropLower 1) ''Borders)
$(deriveBoth (Elm.defaultOptionsDropLower 1) ''PossibleInput)

elmTypes = [
    DefineElm (Proxy :: Proxy GameId), 
    DefineElm (Proxy :: Proxy Position), 
    DefineElm (Proxy :: Proxy SoldierUnit), 
    DefineElm (Proxy :: Proxy PlayerId),
    DefineElm (Proxy :: Proxy PlayerInput),
    DefineElm (Proxy :: Proxy PlayerInputType),
    DefineElm (Proxy :: Proxy PlayerMoveInputError),
    DefineElm (Proxy :: Proxy UnitPositions),
    DefineElm (Proxy :: Proxy Borders),
    DefineElm (Proxy :: Proxy PossibleInput)]

elm = 
    generateElmModule
        ["GameApi"]
        defElmImports
        "app/frontend/src"
        elmTypes
        (Proxy :: Proxy GameApi)
