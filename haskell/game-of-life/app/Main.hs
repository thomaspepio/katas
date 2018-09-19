module Main where

import GameOfLife

random = [[Cell True 0 0, Cell True 1 0, Cell True 2 0, Cell True 3 0, Cell False 4 0]
         ,[Cell False 0 1, Cell False 1 1, Cell False 2 1, Cell True 3 1, Cell False 4 1]
         ,[Cell False 0 2, Cell False 1 2, Cell False 2 2, Cell False 3 2, Cell True 4 2]
         ,[Cell True 0 3, Cell True 1 3, Cell True 2 3, Cell False 3 3, Cell False 4 3]
         ,[Cell True 0 4, Cell True 1 4, Cell False 2 4, Cell False 3 4, Cell False 4 4]]

square = [[Cell False 0 0, Cell False 1 0, Cell False 2 0, Cell False 3 0]
         ,[Cell False 0 1, Cell True 1 1, Cell True 2 1, Cell False 3 1]
         ,[Cell False 0 2, Cell True 1 2, Cell True 2 2, Cell False 3 2]
         ,[Cell False 0 3, Cell False 1 3, Cell False 2 3, Cell False 3 3]]

blinking = [[Cell False 0 0, Cell False 1 0, Cell False 2 0, Cell False 3 0, Cell False 4 0]
           ,[Cell False 0 1, Cell False 1 1, Cell False 2 1, Cell False 3 1, Cell False 4 1]
           ,[Cell False 0 2, Cell True 1 2, Cell True 2 2, Cell True 3 2, Cell False 4 2]
           ,[Cell False 0 3, Cell False 1 3, Cell False 2 3, Cell False 3 3, Cell False 4 3]
           ,[Cell False 0 4, Cell False 1 4, Cell False 2 4, Cell False 3 4, Cell False 4 4]]

main :: IO ()
main = display blinking
    
display :: Board -> IO ()
display board = do
    prettyPrint board
    line <- getLine
    case line of
        "x" -> putStrLn "bye !"
        _   -> display $ frame board

prettyPrint :: Board -> IO ()
prettyPrint board = mapM_ print prettyPrintedCell
    where prettyPrintedCell = fmap (fmap prettyPrintCell) board

prettyPrintCell :: Cell -> String
prettyPrintCell cell  = case cell of
    (Cell True _ _)  -> "O"
    (Cell False _ _) -> " "