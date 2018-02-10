{-# LANGUAGE OverloadedStrings #-}
module InterpreterSpec where

import           Game
import           Interpreter
import           Test.Hspec

spec :: Spec
spec = describe "The bowling game loop" $ do

    describe "An interpreter that translates user inputs into Rolls" $ do
        it "should interpret 'x' as a Strike" $ do
            interpret "x" `shouldBe` Right Strike

        it "should interpret '/' as a Spare" $ do
            interpret "/" `shouldBe` Right Spare

        it "should interpret '.' as a Miss" $ do
            interpret "." `shouldBe` Right Miss

        it "should interpret '5' as a Pin" $ do
            interpret "5" `shouldBe` Right (Reg 5)

        it "should interpret 'END' as a command" $ do
            interpret "END" `shouldBe` Left End

        it "should interpret 'foo' as an unknown command" $ do
            interpret "foo" `shouldBe` Left Unknown
