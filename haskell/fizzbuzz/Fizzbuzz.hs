module Fizzbuzz where

fizzbuzz :: Int -> Either String Int
fizzbuzz n
    | divisibleByFifteen n = Left "Fizzbuzz"
    | divisibleByThree n= Left "Fizz"
    | diisibleByFive n = Left "Buzz"
    | otherwise = Right n

divisibleByThree :: Int -> Bool
divisibleByThree n = divisibleBy n 3

diisibleByFive :: Int -> Bool
diisibleByFive n = divisibleBy n 5

divisibleByFifteen :: Int -> Bool
divisibleByFifteen n = divisibleBy n 15

divisibleBy :: Int -> Int -> Bool
divisibleBy number divisor = rem number divisor == 0
