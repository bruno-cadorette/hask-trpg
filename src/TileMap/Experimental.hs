    {-# LANGUAGE GADTs,  DataKinds, MultiParamTypeClasses, FlexibleInstances #-}

    module TileMap.Experimental where

    data MovementType = Terrestal | Swimming | Combine MovementType MovementType
    data TileType = Grass | Water

    class CanMoveTo (movementType :: MovementType) (tileType :: TileType) 

    instance CanMoveTo Terrestal Grass
    instance CanMoveTo Swimming Water
    instance CanMoveTo a c => CanMoveTo (Combine a b) c
    instance CanMoveTo b c => CanMoveTo (Combine a b) c

    data Unit :: MovementType -> * where
        TerrestalUnit :: Unit Terrestal
        SwimmingUnit :: Unit Swimming
        AmphibianUnit :: Unit (Combine Terrestal Swimming)

    data Tile :: TileType -> * where
        GrassTile :: Tile Grass
        WaterTile :: Tile Water

    test :: CanMoveTo a b => Unit a -> Tile b -> ()
    test _ _ = ()

    test1 = test TerrestalUnit GrassTile
    test2 = test AmphibianUnit GrassTile
    --test3 = test AmphibianUnit WaterTile