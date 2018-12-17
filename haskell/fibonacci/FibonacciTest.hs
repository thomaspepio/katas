module FibonacciTest where

import           Fibonacci
import           Test.Hspec
import           Test.QuickCheck

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


properties :: Spec
properties = describe "Properties of fibonacci" $ do

        it "verifies the property of the length of returned list" $ do
            quickCheck prop_fibonacci_length_zero
            quickCheck prop_fibonacci_length_one
            quickCheck prop_fibonacci_length_two
            quickCheckWith stdArgs { maxSuccess = 17 } prop_fibonacci_length_otherwise

        it "verifies that the last value is the sum of it's two predecessors" $ do
            quickCheckWith stdArgs { maxSuccess = 17 } prop_fibonacci_values


values :: Gen Int
values = choose (3, 20)

prop_fibonacci_length_zero      = length (fibonacci 0) == 1
prop_fibonacci_length_one       = length (fibonacci 1) == 1
prop_fibonacci_length_two       = length (fibonacci 2) == 2
prop_fibonacci_length_otherwise = forAll values $ \x -> length (fibonacci x) == length (fibonacci $ x - 1) + 1

prop_fibonacci_values = forAll values $ \x -> (last $ fibonacci x) == (last $ init $ fibonacci x) + (last $ (init . init) (fibonacci x))

main :: IO ()
main = do
    hspec spec
    hspec properties
