module Fizzbuzz where

fizzbuzz :: Int -> Either String Int
fizzbuzz n
    | divisibleBy 15 n = Left "Fizzbuzz"
    | divisibleBy 3 n = Left "Fizz"
    | divisibleBy 5 n = Left "Buzz"
    | otherwise = Right n

divisibleBy :: Int -> Int -> Bool
divisibleBy divisor number = rem number divisor == 0
