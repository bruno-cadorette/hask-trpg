{-# LANGUAGE FlexibleInstances, GADTs, TypeFamilies, UndecidableInstances, RankNTypes, MultiParamTypeClasses, TypeOperators, DataKinds #-}
module Character.Experimental where
import Data.Kind
import Data.Coerce
import GDP

newtype Rev xs = Rev Defn

newtype CanMoveTo movement tile = IsCompatibleType Defn
data FakeSoldier = FakeSoldier Int MovementType
data FakeEmptyRegion = EmptyRegion Int TileType

canMoveToImpl :: MovementType -> TileType -> Maybe (MovementType, TileType)
canMoveToImpl Terrestrial Grass = Just (Terrestrial, Grass)
canMoveToImpl _ _ = Nothing

canMoveTo :: (MovementType ~~ character) -> (TileType ~~ region) -> Maybe ((MovementType, TileType) ~~ CanMoveTo character region) 
canMoveTo a b = fmap defn $ canMoveToImpl (the a) (the b)
    where 
        canMoveToImpl Terrestrial Grass = Just (Terrestrial, Grass)
        canMoveToImpl _ _ = Nothing



data MovementType = Terrestrial | Swimming | Combine MovementType MovementType
data TileType = Grass | Water

data Unit :: MovementType -> Type where
    TerrestrialUnit :: Unit Terrestrial
    SwimmingUnit :: Unit Swimming
    AmphibianUnit :: Unit (Combine Terrestrial Swimming)

data UnitBox where
    UnitBox :: Unit a -> UnitBox

data Tile :: TileType -> Type where
    GrassTile :: Tile Grass
    WaterTile :: Tile Water

data TileBox where
    TileBox :: Tile a -> TileBox

tileMap :: Bool -> TileBox
tileMap True = TileBox GrassTile
tileMap False = TileBox WaterTile

unitMap :: Bool -> UnitBox
unitMap True = UnitBox TerrestrialUnit
unitMap False = UnitBox SwimmingUnit

applyUnit :: (forall a. Unit a -> b) -> UnitBox -> b
applyUnit f (UnitBox x) = f x 

applyTile :: (forall a. Tile a -> b) -> TileBox -> b
applyTile f (TileBox x) = f x