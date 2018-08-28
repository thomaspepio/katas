{-# LANGUAGE OverloadedStrings #-}
module RomanNumeralsSpec where

import Data.Text
import RomanNumerals
import Test.Hspec

spec :: Spec
spec = describe "Roman numerals kata in haskell" $ do
    
    describe "translate basic roman numerals" $ do
        it "translates 1" $ do
            intToRoman 1 `shouldBe` "I"
        it "translates 5" $ do
            intToRoman 5 `shouldBe` "V"
        it "translates 10" $ do
            intToRoman 10 `shouldBe` "X"
        it "translates 50" $ do
            intToRoman 50 `shouldBe` "L"
        it "translates 100" $ do
            intToRoman 100 `shouldBe` "C"
        it "translates 500" $ do
            intToRoman 500 `shouldBe` "D"
        it "translates 1000" $ do
            intToRoman 1000 `shouldBe` "M"
   
    describe "translates any number" $ do
        it "translates 19" $ do
            intToRoman 19 `shouldBe` "XIX"
        it "translates 74" $ do
            intToRoman 74 `shouldBe` "LXXIV"
        it "translates 152" $ do
            intToRoman 152 `shouldBe` "CLII"
        it "translates 368" $ do
            intToRoman 368 `shouldBe` "CCCLXVIII"
        it "translates 999" $ do
            intToRoman 999 `shouldBe` "CMXCIX"