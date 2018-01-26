module FibonacciTest where

import           Fibonacci
import           Test.Hspec

spec :: Spec
spec = describe "Kata : print the fibonacci numbers an do it in TDD." $ do

    it "should return [1] when asked to print the first number" $ do
        fibonacci 1 `shouldBe` [1]

    it "should return [1, 1] when asked to print two numbers" $ do
        fibonacci 2 `shouldBe` [1, 1]

    it "should return [1, 1, 2] when asked to print three numbers" $ do
        fibonacci 3 `shouldBe` [1, 1, 2]

    it "should return [1, 1, 2, 3, 5, 8, 13, 21, 34, 55] when asked to print ten numbers" $ do
        fibonacci 10 `shouldBe` [1, 1, 2, 3, 5, 8, 13, 21, 34, 55]

main :: IO ()
main = hspec spec
