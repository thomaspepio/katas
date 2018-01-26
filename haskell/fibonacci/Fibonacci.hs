module Fibonacci where

fibonacci :: Int -> [Int]
fibonacci 0 = [0]
fibonacci 1 = [1]
fibonacci n = fibonacci (n-1) ++ [current]
    where minusOne = last (fibonacci (n-1))
          minusTwo = last (fibonacci (n-2))
          current = minusOne + minusTwo
