{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}


module Routes where

import Servant
import Servant.API
import Servant.HTML.Lucid
import Lucid.Base
import DataAccess
import GameState
import Region
import PlayerManagement

type FileApi = "static" :> Raw

type FullApi = Website :<|> FileApi :<|> GameApi
type Website = Get '[HTML] (Html ()) -- :<|> 
   {- "game" :>
        Capture "gameId" GameId :>
            Get '[JSON] GameMap
    -}

type GameApi = 
    "game" :> (
        Get '[JSON] [GameId] :<|>
        Capture "gameId" GameId :> (
            "gameState" :> (
                Get '[JSON] GameMap :<|>
                Capture "playerId" PlayerId :> ReqBody '[JSON] [GameAction] :> Post '[JSON] ()
            ) :<|>
            "borders" :> Get '[JSON] Borders
        )
    )