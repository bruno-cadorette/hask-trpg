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
{-
moveWithMock :: GameMap -> PlayerId -> Move -> Either PlayerMoveInputError GameMap
moveWithMock testedMap playerId = 
        fmap (view (turnInfo.gameMap)) . runLogicTest (Game (borders 15 15) (TurnInfo testedMap mempty 0)) playerId . handleMove

runLogicTest :: Game -> PlayerId -> Sem '[UpdateRegion, ReadMapInfo, CurrentPlayerInfo, State Game, Reader PlayerId, Error PlayerMoveInputError] a -> Either PlayerMoveInputError Game
runLogicTest initialState playerId = 
    run . runError @PlayerMoveInputError . runReader @PlayerId playerId . fmap fst . runState initialState . runLogicPure
-}

mock unitPositions playerId = evalState unitPositions . runReader playerId . runUnitMoving . runCurrentPlayerInfo . runReadMapInfo
runMock unitPositions playerId = run . mock unitPositions playerId
runMockError unitPositions playerId = run . runError @PlayerMoveInputError . mock unitPositions playerId


main :: IO ()
main = do
    let playerId = PlayerId 1
    let otherPlayerId = PlayerId 2
    let homeRegion = RegionId (0,0)

    hspec $ do
        describe "Region tests" $ do      
            it "Given region owned by player, when player selects it, then region is owned by player" $ do
                let gameMap = fromList [(homeRegion, baseSoldier playerId)]
                (runMock gameMap playerId $ isRegionOwnedByPlayer homeRegion) `shouldBe` True

            it "Given region not owned by player, when player selects it, then region is not owned by player" $ do
                let gameMap = fromList [(homeRegion, baseSoldier otherPlayerId)]
                (runMock gameMap playerId $ isRegionOwnedByPlayer homeRegion) `shouldBe` False

            it "Given region dont exist, when player selects it, then region is not owned by player" $ do
                let gameMap = fromList []
                (runMock gameMap playerId $ isRegionOwnedByPlayer homeRegion) `shouldBe` False

            it "Given region is owned by player, when cheking if region is occupied, then region is occupied" $ do
                let gameMap = fromList [(homeRegion, baseSoldier playerId)]
                (runMock gameMap playerId $ isRegionOwnedByPlayer homeRegion) `shouldBe` (runMock gameMap playerId $ isRegionOccupied homeRegion)

            it "Soldier can move to an unoccupied region" $ do
                let gameMap = fromList [(homeRegion, baseSoldier playerId)]
                (runMockError gameMap playerId $ soldierMove homeRegion (RegionId (0, 1))) `shouldBe` Right ()

            it "Soldier can't move farther than their limits" $ do
                let gameMap = fromList [(homeRegion, baseSoldier playerId)]
                (runMockError gameMap playerId $ soldierMove homeRegion (RegionId (2, 1))) `shouldBe` Left (MoveTooMuch homeRegion)