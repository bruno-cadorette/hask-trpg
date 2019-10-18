{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, BlockArguments, TypeApplications, TypeFamilies #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds, RankNTypes, ScopedTypeVariables #-}
module Soldier where

import PlayerManagement
import Control.Lens
import GHC.Generics
import Data.Aeson

data Soldier = Soldier {_hp :: Int, _movement :: Int, _speed :: Int, _faction :: PlayerId} deriving (Show, Generic)
makeLenses ''Soldier
instance FromJSON Soldier
instance ToJSON Soldier

baseSoldier = Soldier 5 2 1
soldier = Soldier 5 2 1 (PlayerId 1)



hitSoldier :: Int -> Soldier -> Maybe Soldier 
hitSoldier damage soldier = 
    let newHp = soldier^.hp - damage in
    if newHp >= 0 then
        Just (set hp newHp soldier)
    else
        Nothing


