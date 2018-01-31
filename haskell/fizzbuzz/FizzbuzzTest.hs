module FizzbuzzTest where

import           Fizzbuzz
import           Test.Hspec

spec :: Spec
spec = describe "Kata : fizzbuzz kata in Haskell" $ do

    it "should return 'Fizz' when given a multiple of 3" $ do
        fizzbuzz 3 `shouldBe` Left "Fizz"

    it "should return 'Buzz' when given a multiple of 5" $ do
        fizzbuzz 5 `shouldBe` Left "Buzz"
        fizzbuzz 10 `shouldBe` Left "Buzz"
        fizzbuzz 50 `shouldBe` Left "Buzz"

    it "should return 'Fizzbuzz' when given 15 or 45" $ do
        fizzbuzz 15 `shouldBe` Left "Fizzbuzz"
        fizzbuzz 45 `shouldBe` Left "Fizzbuzz"

    it "should return 1 when given 1" $ do
        fizzbuzz 1 `shouldBe` Right 1

main :: IO ()
main = hspec spec
