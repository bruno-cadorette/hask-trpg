{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, BlockArguments, TypeApplications, TypeFamilies #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds, RankNTypes, ScopedTypeVariables #-}
module Soldier where

import PlayerManagement
import Control.Lens
import GHC.Generics
import Data.Aeson

data SoldierUnit = SoldierUnit {_hp :: Int, _movement :: Int, _attack :: Int, _range :: Int, _faction :: PlayerId} deriving (Show, Generic)
makeLenses ''SoldierUnit
instance FromJSON SoldierUnit
instance ToJSON SoldierUnit

baseSoldier = SoldierUnit 5 2 1 1
strongSoldier = SoldierUnit 6 2 2 1

class Soldier a where
    soldier :: a -> SoldierUnit

instance Soldier SoldierUnit where
    soldier = id

hitSoldier :: Int -> SoldierUnit -> Maybe SoldierUnit 
hitSoldier damage soldier = 
    let newHp = soldier^.hp - damage in
    if newHp > 0 then
        Just (set hp newHp soldier)
    else
        Nothing


