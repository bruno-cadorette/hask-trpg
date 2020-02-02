{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, BlockArguments, TypeApplications, TypeFamilies #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds, RankNTypes, ScopedTypeVariables #-}


module Character.Stats where

import PlayerManagement
import Control.Lens
import GHC.Generics
import Data.Aeson

newtype ActionId = ActionId Int deriving (Show, Generic)
instance FromJSON ActionId
instance ToJSON ActionId

data Action = Move { range :: Int } | Attack {range :: Int, damage :: Int} deriving (Show, Generic)

mustBeOnEmptySpace (Move _) = True
mustBeOnEmptySpace (Attack _ _) = False

mustBeOnEnemySpace (Move _) = False
mustBeOnEnemySpace (Attack _ _) = True

mustBeOnAllySpace _ = False

instance FromJSON Action
instance ToJSON Action

--data Buff = IsDefending

--data Weapon = Weapon {_weaponAttack :: Int, _weaponRange :: Int} deriving (Show, Generic)
--makeLenses ''Weapon
--instance FromJSON Weapon
--instance ToJSON Weapon

--data Armor = Armor {_defence :: Int} deriving (Show, Generic)
--makeLenses ''Armor

data Hp = Hp {
    _currentHp :: Int, 
    _maxHp :: Int
    } deriving (Show, Generic)
makeLenses ''Hp
instance FromJSON Hp
instance ToJSON Hp

affectCurrentHp :: (Int -> Int) -> Hp -> Hp
affectCurrentHp f (Hp current max) = Hp (f current) max

data Stats = Stats {_strength :: Int, _agility :: Int, _intelligence :: Int, _speed :: Int} deriving (Show, Generic)
makeLenses ''Stats
instance FromJSON Stats
instance ToJSON Stats

data CharacterUnit = CharacterUnit {
    _characterHp :: Hp, 
--    _stats :: Stats, 
    --_damage :: Int,
    _actions :: [(ActionId, Action)],
    --_weapon :: Weapon, 
    --_armor :: Armor, 
    --_buffs :: [Buff], 
    --_movement :: MovementType, 
    _playerId :: PlayerId } deriving (Show, Generic)
makeLenses ''CharacterUnit


instance FromJSON CharacterUnit
instance ToJSON CharacterUnit

class Character a where
    getAttackRange :: a -> Int
    getAttackRange a = 1
    getAttackDamage :: a -> Int
    getAttackDamage a = 1
    getMovementRange :: a -> Int
    getMovementRange a = 1
    getCurrentHp :: a -> Int
    getFaction :: a -> PlayerId
    getActions :: a ->  [Action]

instance Character CharacterUnit where
    getCurrentHp a = a ^.characterHp.currentHp
    getFaction a = a^.playerId
    getActions a = []--a^.actions

baseKnight = CharacterUnit (Hp 10 10) []
strongKnight = baseKnight

getActionFromId (ActionId 0) = Move 1
getActionFromId (ActionId 1) = Attack 1 1 


hitCharacter :: Int -> CharacterUnit -> Maybe CharacterUnit
hitCharacter damage character = 
    --let damage' = extract $ removeDefence damage (view (armor.defence) character) in
    let newHp = affectCurrentHp (\x -> x - damage) (view characterHp character) in
    if (newHp^.currentHp > 0) then
        Just (set characterHp newHp character)
    else
        Nothing