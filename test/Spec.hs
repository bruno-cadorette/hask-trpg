{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, BlockArguments, TypeApplications, TypeFamilies #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds, RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
--stack ghci --ghci-options -isrc --ghci-options -itest Risky:risky-test
import Game.Effects
import Game.Logic
import Region
import Polysemy
import Polysemy.Reader
import Polysemy.Error
import Polysemy.State
import PlayerManagement
import Data.Map
import Data.Either
import Control.Applicative
import Test.QuickCheck
import Test.Hspec
import Soldier
import Control.Lens

mock fctState unitPositions playerId = 
    run
    . runError @PlayerMoveInputError 
    . fctState unitPositions 
    . runReader playerId 
    . runUnitMoving 
    . runCurrentPlayerInfo  
    . runReadMapInfo

testFunctionResult = mock evalState

testState unitPositions playerId = fmap fst . mock runState unitPositions playerId

main :: IO ()
main = do
    let playerId = PlayerId 1
    let otherPlayerId = PlayerId 2
    let homeRegion = Position (0,0)

    hspec $ do
        describe "Region ownership" $ do      
            it "Given region owned by player, when player selects it, then region is owned by player" $ do
                let gameMap = fromList [(homeRegion, baseSoldier playerId)]
                (testFunctionResult gameMap playerId $ isRegionOwnedByPlayer homeRegion) `shouldBe` Right True

            it "Given region not owned by player, when player selects it, then region is not owned by player" $ do
                let gameMap = fromList [(homeRegion, baseSoldier otherPlayerId)]
                (testFunctionResult gameMap playerId $ isRegionOwnedByPlayer homeRegion) `shouldBe` Right False

            it "Given region dont exist, when player selects it, then region is not owned by player" $ do
                let gameMap = fromList []
                (testFunctionResult gameMap playerId $ isRegionOwnedByPlayer homeRegion) `shouldBe` Right False

            it "Given region is owned by player, when cheking if region is occupied, then region is occupied" $ do
                let gameMap = fromList [(homeRegion, baseSoldier playerId)]
                (testFunctionResult gameMap playerId $ isRegionOwnedByPlayer homeRegion) `shouldBe` (testFunctionResult gameMap playerId $ isRegionOccupied homeRegion)

        describe "Soldier moving" $ do
            it "Soldier can move to an unoccupied region" $ do
                let gameMap = fromList [(homeRegion, baseSoldier playerId)]
                (testFunctionResult gameMap playerId $ soldierMove homeRegion (Position (0, 1))) `shouldBe` Right ()

            it "Soldier can't move farther than their limits" $ do
                let gameMap = fromList [(homeRegion, baseSoldier playerId)]
                (testFunctionResult gameMap playerId $ soldierMove homeRegion (Position (2, 1))) `shouldBe` Left (MoveTooMuch homeRegion)

        describe "Soldier hitting" $ do

            it "Soldier can't hit empty region" $ do
                let enemyRegion = Position (2, 1)
                let gameMap = fromList [(homeRegion, baseSoldier playerId)]
                (testFunctionResult gameMap playerId $ soldierAttack homeRegion enemyRegion) `shouldBe` Left (RegionNotOccupied enemyRegion)
            
            it "Soldier who gets hit lose the amount of hp of the attacker attack stat" $ do
                let enemyRegion = Position (0, 1)
                let attackerAttack = 3
                let defenderHp = 5
                let gameMap = fromList [(homeRegion, (baseSoldier playerId)&attack.~attackerAttack), (enemyRegion, (baseSoldier otherPlayerId)&hp.~defenderHp)]
                let newMap = testState gameMap playerId $ soldierAttack homeRegion enemyRegion
                fmap (preview $ at enemyRegion._Just.hp) newMap `shouldBe` Right (Just (defenderHp - attackerAttack))

            it "Soldier who gets hit too hard die" $ do
                let enemyRegion = Position (0, 1)
                let attackerAttack = 100
                let defenderHp = 5
                let gameMap = fromList [(homeRegion, (baseSoldier playerId)&attack.~attackerAttack), (enemyRegion, (baseSoldier otherPlayerId)&hp.~defenderHp)]
                let newMap = testState gameMap playerId $ soldierAttack homeRegion enemyRegion
                fmap (member enemyRegion) newMap `shouldBe` Right False

