module Main where

import           Data.Text
import           Game
import           Interpreter

main :: IO ()
main = do
    putStrLn "> NEW GAME !"
    putStrLn "> How to play : "
    putStrLn ">     type an integer for a regular roll"
    putStrLn ">     type x for a strike"
    putStrLn ">     type / for a spare"
    putStrLn ">     type . for a miss"
    putStrLn ">     type END to end the game"
    putStrLn ">"
    play (Game [])

play :: Game -> IO ()
play (Game rolls) = do
    putStr "> "
    line <- getLine
    let interpreted = interpret (pack line)
    case interpreted of
        Left End -> do
            putStr "> You scored "
            print (score (Game rolls))
        Left Unknown -> do
            putStrLn "> sorry wat ?"
            play (Game rolls)
        Right roll -> play (Game $ rolls ++ [roll])
