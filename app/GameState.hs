{-# LANGUAGE DeriveGeneric #-}
module GameState where

import Data.Aeson
import Data.Map 
import GHC.Generics
import Region

data GameAction = Move {origin :: RegionId, destination :: RegionId, troops :: Integer} deriving(Generic, Show)

instance FromJSON GameAction
instance ToJSON GameAction

adjustGet :: Ord k => (a -> a) -> k -> Map k a -> (Maybe a, Map k a)
adjustGet f k m = 
    case Data.Map.lookup k m of
        Just a  -> (Just a, Data.Map.insert k (f a) m)
        Nothing -> (Nothing, m)

move (Move originId destinationId troopsNumber) (GameMap m) = 
    let (x, m') = adjustGet (\(RegionInfo f p) -> RegionInfo f (p - troopsNumber)) originId m
    in GameMap $ maybe m' (\(RegionInfo f1 _) -> adjust (invasion f1 troopsNumber) destinationId m') x
    
invasion attackerFaction attackerTroops (RegionInfo defenderFaction defenderTroops)
    | attackerFaction == defenderFaction =
        RegionInfo defenderFaction (attackerTroops + defenderTroops)
    | defenderTroops - attackerTroops < 0 =
        RegionInfo attackerFaction (attackerTroops - defenderTroops)
    | otherwise =
        RegionInfo defenderFaction (defenderTroops - attackerTroops)

reinforce = GameMap . Data.Map.map (\(RegionInfo f p) -> RegionInfo f (if f == FactionId 0 then p else p + 1)) . gameMapToMap

