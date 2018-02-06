{-# LANGUAGE OverloadedStrings #-}
module InterpreterSpec where

import           Game
import           Interpreter
import           Test.Hspec

spec :: Spec
spec = describe "The bowling game loop" $ do

    describe "An interpreter that translates user inputs into Rolls" $ do
        it "should interpret 'x' as a Strike" $ do
            interpret "x" `shouldBe` Just Strike

        it "should interpret '/' as a Spare" $ do
            interpret "/" `shouldBe` Just Spare

        it "should interpret '.' as a Miss" $ do
            interpret "." `shouldBe` Just Miss

        it "should interpret '5' as a Pin" $ do
            interpret "5" `shouldBe` Just (Reg 5)

        it "should interpret 'foo' as a Nothing" $ do
            interpret "foo" `shouldBe` Nothing
