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
import Control.Lens

moveWithMock :: GameMap -> PlayerId -> Move -> Either PlayerMoveInputError GameMap
moveWithMock testedMap playerId = 
        fmap (view (turnInfo.gameMap)) . runLogicTest (Game (borders 15 15) (TurnInfo testedMap mempty 0)) playerId . handleMove

runLogicTest :: Game -> PlayerId -> Sem '[UpdateRegion, ReadMapInfo, CurrentPlayerInfo, State Game, Reader PlayerId, Error PlayerMoveInputError] a -> Either PlayerMoveInputError Game
runLogicTest initialState playerId = 
    run . runError @PlayerMoveInputError . runReader @PlayerId playerId . fmap fst . runState initialState . runLogicPure

main :: IO ()
main = hspec $ do
    describe "Move tests" $ do
        let playerId = PlayerId 1
        let homeRegion = RegionId "1"
        let awayRegion = RegionId "2"
        let playerArmy = Army 8

        it "Given empty area, when player fully attacks, then player conquer" $ do
            let gameMap = fromList [
                    (homeRegion, RegionInfo (Just playerId) playerArmy),
                    (awayRegion, RegionInfo Nothing (Army 0))
                    ]
            let expectedResult = fromList [
                    (homeRegion, RegionInfo (Just playerId) (Army 0)),
                    (awayRegion, RegionInfo (Just playerId) playerArmy)
                    ]

            moveWithMock gameMap playerId (Move homeRegion awayRegion playerArmy) `shouldBe` (Right expectedResult) 
        it "Given owned area, when player move army, army doesnt lose" $ do
            let gameMap = fromList [
                    (homeRegion, RegionInfo (Just playerId) playerArmy),
                    (awayRegion, RegionInfo (Just playerId) (Army 2))
                    ]
            let expectedResult = fromList [
                    (homeRegion, RegionInfo (Just playerId) (Army 0)),
                    (awayRegion, RegionInfo (Just playerId) (Army 10))
                    ]
                
            moveWithMock gameMap playerId (Move homeRegion awayRegion playerArmy) `shouldBe` (Right expectedResult) 

        it "Given neutral occupied area, when player attacks with bigger army, then player conquer" $ do
            let gameMap = fromList [
                    (homeRegion, RegionInfo (Just playerId) playerArmy),
                    (awayRegion, RegionInfo Nothing (Army 2))
                    ]
            let expectedResult = fromList [
                    (homeRegion, RegionInfo (Just playerId) (Army 0)),
                    (awayRegion, RegionInfo (Just playerId) (Army 6))
                    ]
                
            moveWithMock gameMap playerId (Move homeRegion awayRegion playerArmy) `shouldBe` (Right expectedResult) 

        it "Given enemy's area is populous, when player attacks with smaller army, then enemy area lose some units" $ do
            let gameMap = fromList [
                    (homeRegion, RegionInfo (Just playerId) playerArmy),
                    (awayRegion, RegionInfo Nothing (Army 10))
                    ]
            let expectedResult = fromList [
                    (homeRegion, RegionInfo (Just playerId) (Army 0)),
                    (awayRegion, RegionInfo Nothing (Army 2))
                    ]
                
            moveWithMock gameMap playerId (Move homeRegion awayRegion playerArmy) `shouldBe` (Right expectedResult) 

        it "Given neutral area, when player try to move from it, then exception happend" $ do
            let gameMap = fromList [
                    (homeRegion, RegionInfo Nothing playerArmy),
                    (awayRegion, RegionInfo Nothing (Army 10))
                    ]

            moveWithMock gameMap playerId (Move homeRegion awayRegion playerArmy) `shouldBe` (Left (NotPlayerOwned homeRegion))

        it "Given empty map, when player try to move, then exception happend" $ do
            let gameMap = fromList []
            moveWithMock gameMap playerId (Move homeRegion awayRegion playerArmy) `shouldBe` (Left (RegionDontExist homeRegion))
