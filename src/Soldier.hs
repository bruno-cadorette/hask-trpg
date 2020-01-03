{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, BlockArguments, TypeApplications, TypeFamilies #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds, RankNTypes, ScopedTypeVariables #-}
module Soldier where

import PlayerManagement
import Control.Lens
import GHC.Generics
import Data.Aeson
import Data.Monoid
import Data.Range

deriving instance Generic BoundType
instance ToJSON BoundType
instance FromJSON BoundType

deriving instance Generic (Bound a)
instance ToJSON a => ToJSON (Bound a)
instance FromJSON a => FromJSON (Bound a)

deriving instance Generic (Range a)
instance ToJSON a => ToJSON (Range a)
instance FromJSON a => FromJSON (Range a)


data MovementType = Walking | Swiming | Flying deriving (Show, Generic)
instance FromJSON MovementType
instance ToJSON MovementType


data Buff = BuffStrength Int | BuffIntelligence deriving (Show, Generic)
instance FromJSON Buff
instance ToJSON Buff

data BaseStats = BaseStats {_baseHp :: Int, _baseMovement :: Int, _baseStrength :: Int, _baseMovementType :: MovementType} deriving (Show, Generic)
makeLenses ''BaseStats
instance FromJSON BaseStats
instance ToJSON BaseStats

data DamageType a = Physical a | Elemental a deriving (Show, Generic)
removeDefence (Physical a) (Physical b) = Physical (a - b)
removeDefence (Elemental a) (Elemental b) = Elemental (a - b)
removeDefence a _ = a

extract (Physical a) = a
extract (Elemental a) = a

instance FromJSON a => FromJSON (DamageType a)
instance ToJSON a => ToJSON (DamageType a)

data Weapon = Weapon {_weaponAttack :: DamageType (Range Int), _weaponRange :: Range Int} deriving (Show, Generic)
makeLenses ''Weapon
instance FromJSON Weapon
instance ToJSON Weapon

data Armor = Armor {_defence :: DamageType Int} deriving (Show, Generic)
makeLenses ''Armor
instance FromJSON Armor
instance ToJSON Armor



data Hp = Hp {_currentHp :: Int, _maxHp :: Int} deriving (Show, Generic)
makeLenses ''Hp
instance FromJSON Hp
instance ToJSON Hp

affectCurrentHp :: (Int -> Int) -> Hp -> Hp
affectCurrentHp f (Hp current max) = Hp (f current) max

data Stats = Stats {_strength :: Int, _agility :: Int, _intelligence :: Int, _speed :: Int} deriving (Show, Generic)
makeLenses ''Stats
instance FromJSON Stats
instance ToJSON Stats


data CharacterUnit = CharacterUnit {_characterHp :: Hp, _stats :: Stats, _weapon :: Weapon, _armor :: Armor, _buffs :: [Buff],  _movement :: MovementType, _playerId :: PlayerId } deriving (Show, Generic)
makeLenses ''CharacterUnit
instance FromJSON CharacterUnit
instance ToJSON CharacterUnit

noArmor = Armor (Physical 0)
knife = Weapon (Physical (1 +=+ 2)) (1 +=+ 1)
axe = Weapon (Physical (2 +=+ 3)) (1 +=+ 1)

data NamedAction = WeaponAttack | Heal | Fireball
data ConcreteAction = Data

lowLevel = BaseStats 5 2 1 Walking

baseKnight = CharacterUnit (Hp 5 5) (Stats 1 1 1 1) knife noArmor [] Walking
strongKnight = CharacterUnit (Hp 5 5) (Stats 1 1 1 1) knife noArmor [] Walking

totalStrength character = 
    (character^.stats.strength) + getSum (foldMap (\case {BuffStrength i -> Sum i; _ -> mempty}) (character^.buffs))

affectStats str (Physical x) = Physical (fmap (+ str) x)

applyBuff :: Buff -> Stats -> Stats
applyBuff (BuffStrength i) stats = stats&strength^~i


hitCharacter :: Int -> CharacterUnit -> Maybe CharacterUnit
hitCharacter damage character = 
    --let damage' = extract $ removeDefence damage (view (armor.defence) character) in
    let newHp = affectCurrentHp (\x -> x - damage) (view characterHp character) in
    if (newHp^.currentHp > 0) then
        Just (set characterHp newHp character)
    else
        Nothing