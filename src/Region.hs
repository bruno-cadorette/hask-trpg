{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, BlockArguments, TypeApplications, TypeFamilies #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds, RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
module Region where

import PlayerManagement
import Data.Aeson
import Data.Map 
import GHC.Generics
import Control.Lens
import Data.List
import Data.Ord
import Polysemy
import Polysemy.State

newtype RegionId = RegionId (Int, Int) deriving (Show, Eq, Ord, FromJSON, ToJSON, ToJSONKey, FromJSONKey)

newtype Borders = Borders (Map RegionId [RegionId]) deriving (FromJSON, ToJSON)
newtype Army = Army Int deriving (Show, Eq, Ord, FromJSON, ToJSON, Num)

data Soldier = Soldier {_hp :: Int, _movement :: Int, _speed :: Int, _soldierFaction :: PlayerId} deriving (Show, Generic)
makeLenses ''Soldier
instance FromJSON Soldier
instance ToJSON Soldier

soldier = Soldier 5 2 1 (PlayerId 1)

data RegionInfo = RegionInfo {_faction :: Maybe PlayerId, _population :: Army, _currentSoldier :: Maybe Soldier} deriving(Show, Generic)

instance FromJSON RegionInfo
instance ToJSON RegionInfo


makeLenses ''RegionInfo



data UnitAction m a where
    LoseHP :: Int -> RegionId -> UnitAction m () 
    MoveM :: RegionId -> RegionId -> UnitAction m ()

makeSem ''UnitAction


type UnitPositions = Map RegionId Soldier
type GameMap = Map RegionId RegionInfo


hitSoldier :: Int -> Soldier -> Maybe Soldier 
hitSoldier damage soldier = 
    let newHp = soldier^.hp - damage in
    if newHp >= 0 then
        Just (set hp newHp soldier)
    else
        Nothing


runUnitMoving ::Members '[State UnitPositions] r => InterpreterFor UnitAction r
runUnitMoving = interpret $ \case
    (MoveM origin destination) -> do
        playerToMove <- gets (Data.Map.lookup origin) 
        case playerToMove of
            Just p -> 
                modify (Data.Map.insert destination p)
            Nothing ->
                pure () -- Maybe use Data.Map.Justified instead of map for this???
    (LoseHP damage location) ->
        modify (Data.Map.update (hitSoldier damage) location)


baseRegions :: Int -> Int -> GameMap
baseRegions x y = fromList $ fmap (\id -> (id, RegionInfo (getFaction id) 1 Nothing)) $ allRegionRegionId mkRegionId x y

getFaction :: RegionId -> Maybe PlayerId
getFaction (RegionId (2,2)) = Just $ PlayerId 1
getFaction (RegionId (6,8)) = Just $ PlayerId 2
getFaction _     = Nothing

allRegionRegionId f x y = do
    x' <- [0..x]
    fmap (f x') [0..y]


mkRegionId :: Int -> Int -> RegionId
mkRegionId x y = RegionId $ (x,y)

neighboor maxX maxY (x, y) = (mkRegionId x y , allNeighboor)
    where
        allNeighboor = do
            x' <- [(max 0 $ x-1)..(min maxX $ x+1)]
            y' <- [(max 0 $ y-1)..(min maxY $ y+1)]
            return $ mkRegionId x' y'
 

distance :: RegionId -> RegionId -> Int
distance (RegionId (x1, y1)) (RegionId (x2,y2)) = abs (x2 - x1) + abs (y2 - y1) 


borders :: Int -> Int -> Borders
borders x y = Borders $ fromList $ fmap (neighboor x y) $ allRegionRegionId (,) x y

findClosest :: RegionId -> [RegionId] -> RegionId
findClosest origin = minimumBy (comparing (distance origin))