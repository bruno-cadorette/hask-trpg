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

data Action = Move | Attack deriving (Show, Generic)

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
    _stats :: Stats, 
    _damage :: Int,
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
    getAttackDamage :: a -> Int
    getCurrentHp :: a -> Int
    getMovementRange :: a -> Int
    getFaction :: a -> PlayerId

instance Character CharacterUnit where
    getAttackRange a = 1
    getAttackDamage a = a^.damage
    getCurrentHp a = a ^.characterHp.currentHp
    getMovementRange a = 2
    getFaction a = a^.playerId

baseKnight = CharacterUnit (Hp 10 10) (Stats 1 1 1 1) 2
strongKnight = baseKnight



hitCharacter :: Int -> CharacterUnit -> Maybe CharacterUnit
hitCharacter damage character = 
    --let damage' = extract $ removeDefence damage (view (armor.defence) character) in
    let newHp = affectCurrentHp (\x -> x - damage) (view characterHp character) in
    if (newHp^.currentHp > 0) then
        Just (set characterHp newHp character)
    else
        Nothing