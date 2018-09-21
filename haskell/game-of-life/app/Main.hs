module Main where

import GameOfLife

square = [ Cell True 1 1, Cell True 2 1, Cell True 1 2, Cell True 2 2 ]
blinking = [ Cell True 0 1, Cell True 1 1, Cell True 2 1 ]

main :: IO ()
main = display blinking
    
display :: Board -> IO ()
display board = do
    print board
    line <- getLine
    case line of
        "x" -> putStrLn "bye !"
        _   -> display $ frame board