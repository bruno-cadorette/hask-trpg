module Ai (generateMoves) where

import Data.Map as Map
import Control.Lens
import Region
import Data.Maybe
import Data.Bifunctor
import PlayerManagement
import Game.Logic

-- This module, sadly, does not use any cool deep learning framework

generateMoves :: PlayerId -> Map RegionId RegionInfo -> [Move]
generateMoves player gameMap = 
    uncurry findMoves $ bimap Map.keys Map.keys $ Map.partition (\x -> Just player == view faction x) $ allOccupied gameMap

allOccupied =  Map.filter (isJust . view faction)

findMoves :: [RegionId] -> [RegionId] -> [Move]
findMoves aiMap enemies = fmap (\ai -> Move ai (findClosest ai enemies) (Army 1)) aiMap