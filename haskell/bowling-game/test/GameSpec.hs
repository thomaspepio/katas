{-# LANGUAGE OverloadedStrings #-}
module GameSpec where

import           Game
import           Test.Hspec

spec :: Spec
spec = describe "Computing bowling game scores" $ do

        describe "base values for scoring rolls" $ do
            it "scores 0 if the player misses" $ do
                score' Miss `shouldBe` 0
            it "scores the number of pins knocked" $ do
                score' (Reg 9) `shouldBe` 9
            it "scores 10 if the player does a spare" $ do
                score' Spare `shouldBe` 10
            it "scores 10 if the player does a strike" $ do
                score' Strike `shouldBe` 10

        describe "scoring frames" $ do

            describe "two frames" $ do
                it "scores a spare and two rolls (two frames)" $ do
                    score (Game [Reg 1, Spare, Reg 1, Reg 1]) `shouldBe` (11 + 1 + 1)
                it "scores a strike and two rolls (two frames)" $ do
                    score (Game [Strike, Reg 1, Reg 1]) `shouldBe` (12 + 1 + 1)

            describe "three frames" $ do
                it "scores two spares and two rolls (three frames)" $ do
                    score (Game [Reg 1, Spare, Reg 1, Spare, Reg 1, Reg 1]) `shouldBe` (11 + 11 + 1 + 1)
                it "scores two strikes and two rolls (three frames)" $ do
                    score (Game [Strike, Strike, Reg 1, Reg 1]) `shouldBe` (21 + 12 + 1 + 1)

            describe "four frames" $ do
                it "scores three spares and two rolls (four frames)" $ do
                    score (Game [Reg 1, Spare, Reg 1, Spare, Reg 1, Spare, Reg 1, Reg 1]) `shouldBe` (11 + 11 + 11 + 1 + 1)
                it "scores three strikes and two rolls (four frames)" $ do
                    score (Game [Strike, Strike, Strike, Reg 1, Reg 1]) `shouldBe` (30 + 21 + 12 + 1 + 1)

        describe "scoring whole games" $ do
            it "all rolls are misses" $ do
                score (Game $ replicate 20 Miss) `shouldBe` 0
            it "all rolls are regular"$ do
                score (Game $ replicate 20 (Reg 1)) `shouldBe` 20
            it "a game of spares" $ do
                score (Game $ concat (replicate 10 [Reg 5, Spare]) ++ [Reg 5]) `shouldBe` 150
            it "all rolls are strikes" $ do
                score (Game $ replicate 12 Strike) `shouldBe` 300
