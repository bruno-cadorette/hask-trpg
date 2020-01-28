{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PlayerManagement where

import Data.Aeson
import Servant
import Control.Concurrent.STM
import Control.Concurrent.STM.TArray
import Data.Ix
import Data.Array.MArray
import Data.Maybe
import Data.Bifunctor
import Data.Foldable


newtype PlayerId = PlayerId Integer deriving (Eq, Ord, Show, Ix, ToJSON, FromJSON, FromHttpApiData)

newtype PlayerCache' a = PlayerCache' ()
newtype PlayerCache a = PlayerCache (TArray PlayerId (Maybe a))

writeCache :: PlayerCache a -> PlayerId -> a -> STM ()
writeCache (PlayerCache cache) pid x =
    writeArray cache pid (Just x)

cacheIsFull :: PlayerCache a -> STM Bool
cacheIsFull (PlayerCache cache) = fmap (all isJust) (getElems cache)

emptyCache :: PlayerCache a -> STM ()
emptyCache (PlayerCache cache) = do
    bounds <- getBounds cache
    traverse_ (\i -> writeArray cache i Nothing) $ range bounds

getCompletedCache :: PlayerCache a -> STM [(PlayerId, a)]
getCompletedCache (PlayerCache cache) = fmap (mapMaybe sequenceA) (getAssocs cache)


addToCache :: PlayerCache a -> PlayerId -> a -> STM (Maybe [(PlayerId, a)])
addToCache cache pid x = do
    writeCache cache pid x
    isFull <- cacheIsFull cache
    if isFull then do
        result <- getCompletedCache cache
        emptyCache cache
        return $ Just result
    else
        return Nothing


basePlayerCache :: Integer -> STM (PlayerCache a)
basePlayerCache maxPlayers = PlayerCache <$> newArray (PlayerId 1, PlayerId maxPlayers) Nothing
