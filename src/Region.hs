{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Data.Aeson.Types
import Data.Map 
import GHC.Generics
import Control.Lens
import Data.List
import Data.Text
import Data.Text.Read
import Data.Ord
import Polysemy
import Polysemy.State

    

newtype RegionId = RegionId (Int, Int) deriving (Show, Eq, Ord, ToJSONKey, FromJSONKey)
instance ToJSON RegionId where
    toJSON (RegionId (x, y)) = String $ pack $ (show x) <> "_" <> (show y)
instance FromJSON RegionId where
    parseJSON v = withText "RegionId" actualParsing v
            

actualParsing id =
    let (x, y) = breakOn "_" id in
    case (\x y ->  RegionId (fst x, fst y)) <$> decimal x <*> decimal (Data.Text.tail y) of
        Right regionId -> pure regionId
        Left error -> fail error

newtype Borders = Borders (Map RegionId [RegionId]) deriving (FromJSON, ToJSON)
newtype Army = Army Int deriving (Show, Eq, Ord, FromJSON, ToJSON, Num)

data Soldier = Soldier {_hp :: Int, _movement :: Int, _speed :: Int, _faction :: PlayerId} deriving (Show, Generic)
makeLenses ''Soldier
instance FromJSON Soldier
instance ToJSON Soldier

baseSoldier = Soldier 5 2 1
soldier = Soldier 5 2 1 (PlayerId 1)


data UnitAction m a where
    LoseHP :: Int -> RegionId -> UnitAction m () 
    Move :: RegionId -> RegionId -> UnitAction m ()

makeSem ''UnitAction



data ReadMapInfo m a where
    --GetRegionInfo :: RegionId -> ReadMapInfo m RegionInfo
    GetUnit :: RegionId -> ReadMapInfo m (Maybe Soldier)


makeSem ''ReadMapInfo

type UnitPositions = Map RegionId Soldier


hitSoldier :: Int -> Soldier -> Maybe Soldier 
hitSoldier damage soldier = 
    let newHp = soldier^.hp - damage in
    if newHp >= 0 then
        Just (set hp newHp soldier)
    else
        Nothing


runUnitMoving ::Members '[State UnitPositions] r => InterpreterFor UnitAction r
runUnitMoving = interpret $ \case
    (Move origin destination) -> do
        playerToMove <- gets (Data.Map.lookup origin) 
        case playerToMove of
            Just p -> 
                modify (Data.Map.insert destination p)
            Nothing ->
                pure () -- Maybe use Data.Map.Justified instead of map for this???
    (LoseHP damage location) ->
        modify (Data.Map.update (hitSoldier damage) location)



runReadMapInfo :: Members '[State UnitPositions] r => InterpreterFor ReadMapInfo r
runReadMapInfo = interpret $ \(GetUnit regionId) -> gets (Data.Map.lookup regionId)


baseUnitPositions :: UnitPositions
baseUnitPositions = fromList [(RegionId (2,2), baseSoldier (PlayerId 1)), (RegionId (6,8), baseSoldier (PlayerId 2)) ]

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