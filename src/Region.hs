{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Data.Ord
import Polysemy
import Polysemy.State
import Soldier
import Data.Coerce
import Debug.Trace
import Servant
import qualified Data.Text as Text



newtype Position = Position (Int, Int) deriving (Show, Eq, Ord, ToJSONKey, FromJSONKey)

instance FromHttpApiData Position where
  parseQueryParam txt = 
    case Text.splitOn "_" txt of
      [x, y] -> Right $ Position (read (Text.unpack x), read (Text.unpack y))
      _ -> Left "Cannot parse Position"

newtype Borders = Borders (Map Position [Position]) deriving (Show)

newtype UnitPositions = UnitPositions (Map Position SoldierUnit) deriving (Show)

data Game = Game { _gameBorders :: Borders, _turnNumber :: Int, _unitPositions :: UnitPositions} deriving (Show)

makeLenses ''Game



newtype Army = Army Int deriving (Show, Eq, Ord, Num)
class Region a where
    getPosition :: a -> Position

instance Region Position where
    getPosition = id

instance Region EmptyRegion where
    getPosition (EmptyRegion regionId) = regionId

instance Region TargetSoldier where
    getPosition (TargetSoldier regionId _) = regionId
    

instance Soldier TargetSoldier where
    soldier (TargetSoldier _ soldier) = soldier

newtype EmptyRegion = EmptyRegion Position deriving(Show)
data TargetSoldier = TargetSoldier Position SoldierUnit deriving (Show)

makeLenses ''TargetSoldier 

data UnitAction m a where
    LoseHP :: Int -> TargetSoldier -> UnitAction m () 
    Move :: TargetSoldier -> EmptyRegion -> UnitAction m ()

makeSem ''UnitAction

data ReadMapInfo m a where
    GetUnit :: Position -> ReadMapInfo m (Either EmptyRegion TargetSoldier)

makeSem ''ReadMapInfo

modifyUnitPositions :: Member (State Game) r => (UnitPositions -> UnitPositions) -> Sem r ()
modifyUnitPositions = modify . over unitPositions 

runUnitMoving ::Members '[State Game] r => InterpreterFor UnitAction r
runUnitMoving = interpret $ \case
    Move (TargetSoldier orign playerToMove) (EmptyRegion destination) -> do
        modifyUnitPositions (coerce (Data.Map.delete orign . Data.Map.insert destination playerToMove))
    LoseHP damage (TargetSoldier location _) -> 
        modifyUnitPositions (coerce (Data.Map.update (hitSoldier damage) location))


runReadMapInfo :: Members '[State Game] r => InterpreterFor ReadMapInfo r
runReadMapInfo = interpret $ \(GetUnit regionId) -> 
    gets (
        maybe (Left $ EmptyRegion regionId) (Right . TargetSoldier regionId). 
        Data.Map.lookup regionId . 
        coerce . 
        view unitPositions)


baseUnitPositions :: UnitPositions
baseUnitPositions = UnitPositions $ fromList [(Position (2,2), strongSoldier 0 (PlayerId 1)), (Position (6,8), baseSoldier 1 (PlayerId 2)) ]

allRegionPosition f x y = do
    x' <- [0..x]
    fmap (f x') [0..y]


mkPosition :: Int -> Int -> Position
mkPosition x y = Position (x,y)

neighboor maxX maxY (x, y) = (mkPosition x y , allNeighboor)
    where
        allNeighboor = do
            x' <- [(max 0 $ x-1)..(min maxX $ x+1)]
            fmap (mkPosition x') [(max 0 $ y-1)..(min maxY $ y+1)]
 

distance :: Position -> Position -> Int
distance (Position (x1, y1)) (Position (x2,y2)) = abs (x2 - x1) + abs (y2 - y1) 

directNeighboor :: Position -> [Position]
directNeighboor (Position (a,b)) = [Position (a + 1, b), Position (a - 1, b), Position (a, b + 1), Position (a, b - 1)]

borders :: Int -> Int -> Borders
borders x y = Borders $ fromList $ neighboor x y <$> allRegionPosition (,) x y

findClosest :: Position -> [Position] -> Position
findClosest origin = minimumBy (comparing (distance origin))

extendedNeighboor n r = tail $ nub $ concat $ Data.List.take (n + 1) $ iterate (concatMap directNeighboor) [r]