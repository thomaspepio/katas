module Fizzbuzz where

fizzbuzz :: Int -> Either String Int
fizzbuzz n
    | divisibleBy n 15 = Left "Fizzbuzz"
    | divisibleBy n 3 = Left "Fizz"
    | divisibleBy n 5 = Left "Buzz"
    | otherwise = Right n

divisibleBy :: Int -> Int -> Bool
divisibleBy number divisor = rem number divisor == 0
