{-# LANGUAGE OverloadedStrings #-}
module GameSpec where

import           Game
import           Test.Hspec

spec :: Spec
spec = describe "Computing bowling game scores" $ do

        describe "base values for scoring rolls" $ do
            it "scores 0 if the player misses" $ do
                scoreThrow Miss `shouldBe` 0
            it "scores the number of pins knocked" $ do
                scoreThrow (Reg 9) `shouldBe` 9
            it "scores 10 if the player does a spare" $ do
                scoreThrow Spare `shouldBe` 10
            it "scores 10 if the player does a strike" $ do
                scoreThrow Strike `shouldBe` 10

        describe "scoring whole games" $ do
            it "all rolls are misses" $ do
                score (Game $ replicate 20 Miss) `shouldBe` 0
            it "all rolls are regular"$ do
                score (Game $ replicate 20 (Reg 1)) `shouldBe` 20
            it "a game of spares" $ do
                score (Game $ concat (replicate 10 [Reg 5, Spare]) ++ [Reg 5]) `shouldBe` 150
            it "all rolls are strikes" $ do
                score (Game $ replicate 12 Strike) `shouldBe` 300
            it "almost all rolls are strikes" $ do
                score (Game $ replicate 10 Strike ++ [Reg 3, Reg 5]) `shouldBe` 281
