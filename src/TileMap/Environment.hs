{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}


module TileMap.Environment where
import Data.Map
import Data.Aeson
import GHC.Generics
import Debug.Trace

data TerrainType = Grass | Water | Wall deriving (Generic, Show)

instance FromJSON TerrainType
instance ToJSON TerrainType

newtype RegionId = RegionId (Int, Int) deriving (Show, Eq, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype Borders = Borders (Map RegionId TerrainType) deriving (FromJSON, ToJSON, Show, Semigroup, Monoid)

mkTileId x y = RegionId (x, y)

line x1 x2 y t = fmap (\x -> (mkTileId x y, t)) [x1..x2]
column y1 y2 x t = fmap (\y -> (mkTileId x y, t)) [y1..y2]
square (x1, y1) (x2, y2) t = (\x y -> (mkTileId x y, t)) <$> [x1 .. x2] <*> [y1 .. y2]

water :: Int -> Int -> Borders
water x y = Borders $ fromList $ line 0 x 0 Water ++ line 0 x y Water ++ column 0 y 0 Water ++ column 0 y x Water

grass :: Int -> Int -> Borders
grass x y = Borders $ fromList $ square (0, 0) (x, y) Grass

borders = water <> grass