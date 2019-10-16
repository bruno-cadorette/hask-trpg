module Game.TroopOrder where

import Game.Logic
import PlayerManagement
import qualified Data.Map as Map
import Data.Foldable
import Data.Ord
import Data.Bifunctor
import Data.List.NonEmpty as NonEmpty
import Data.Coerce
import Data.List
import Region


data Placement a = Placement { placed :: Int, remainings :: (NonEmpty a)}
extractTop = NonEmpty.head . remainings
createPlacement = fmap (Placement 0) . nonEmpty

type RemainingPlacements k a = Map.Map k (Placement a) 

class Placed a where
    getScore :: a -> Int

instance Placed PlayerInput where
    getScore = const 1

instance Placed Int where
    getScore = id


nextPlacement :: Placed a => RemainingPlacements k a -> Maybe (k, a)
nextPlacement = fmap (second extractTop . maximumBy (comparing (getScore . extractTop . snd))). nonEmpty . Map.assocs

updatePlacements :: Placed a => a -> Placement a -> Maybe (Placement a)
updatePlacements v (Placement p r) = 
    fmap (Placement ((getScore v) + p)) $ nonEmpty $ NonEmpty.tail r

place :: (Ord k, Placed a) => RemainingPlacements k a -> [(k, a)]
place = Data.List.unfoldr algorithm
    where 
        algorithm xs = fmap (\(k,v) -> ((k,v), Map.update (updatePlacements v) k xs)) $ nextPlacement xs
