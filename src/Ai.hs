module Ai (generateMove) where

import Data.Map as Map
import Control.Lens
import Region
import Data.Maybe
import Data.Bifunctor
import PlayerManagement
import Game.Logic
import Character.Stats
import TileMap.Environment
import Data.List.NonEmpty as NonEmpty

-- This module, sadly, does not use any cool deep learning framework

generateMove :: PlayerId -> Map RegionId CharacterUnit -> Maybe PlayerInput
generateMove player = 
    fmap (NonEmpty.head) . nonEmpty . uncurry findMoves . bimap Map.keys Map.keys . Map.partition (\x -> player == getFaction x)

findMoves :: [RegionId] -> [RegionId] -> [PlayerInput]
findMoves aiMap enemies = fmap (\aiUnit -> PlayerInput Movement aiUnit (findDirection aiUnit $ findClosest aiUnit enemies)) aiMap

findDirection (RegionId (o1, o2)) (RegionId (d1, d2)) = RegionId (o1 + dir o1 d1, o2 + dir o2 d2)
    where 
        dir a b
            | a > b = -1
            | a < b = 1
            | otherwise = 0