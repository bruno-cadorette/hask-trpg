module Ai (generateMoves) where

import Data.Map as Map
import Control.Lens
import Region
import Data.Maybe
import Data.Bifunctor
import PlayerManagement
import Game.Logic
import Soldier

-- This module, sadly, does not use any cool deep learning framework

generateMoves :: PlayerId -> Map RegionId SoldierUnit -> [PlayerInput]
generateMoves player gameMap = 
    uncurry findMoves $ bimap Map.keys Map.keys $ Map.partition (\x -> player == x^.faction) gameMap

findMoves :: [RegionId] -> [RegionId] -> [PlayerInput]
findMoves aiMap enemies = fmap (\aiUnit -> PlayerInput Movement aiUnit (findClosest aiUnit enemies)) aiMap