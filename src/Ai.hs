module Ai (generateMove) where

import Data.Map as Map
import Control.Lens
import Region
import Data.Maybe
import Data.Bifunctor
import PlayerManagement
import Game.Logic
import Soldier
import Data.List.NonEmpty as NonEmpty

-- This module, sadly, does not use any cool deep learning framework

generateMove :: PlayerId -> Map Position SoldierUnit -> Maybe PlayerInput
generateMove player = 
    fmap (NonEmpty.head) . nonEmpty . uncurry findMoves . bimap Map.keys Map.keys . Map.partition (\x -> player == x^.faction)

findMoves :: [Position] -> [Position] -> [PlayerInput]
findMoves aiMap enemies = fmap (\aiUnit -> PlayerInput Movement aiUnit (findDirection aiUnit $ findClosest aiUnit enemies)) aiMap

findDirection (Position (o1, o2)) (Position (d1, d2)) = Position (o1 + dir o1 d1, o2 + dir o2 d2)
    where 
        dir a b
            | a > b = -1
            | a < b = 1
            | otherwise = 0