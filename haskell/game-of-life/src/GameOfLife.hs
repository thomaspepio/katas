module GameOfLife
    ( 
        Cell(..), Board(..),
        frame,
        actOnCell,
        leftNeighbor, rightNeighbor,
        upperLeftNeighbor, upperNeighbor, upperRightNeighbor,
        lowerLeftNeighbor, lowerNeighbor, lowerRightNeighbor
    ) 
where

import Data.Maybe

data Cell = Cell { state :: Bool
                 , x :: Int
                 , y :: Int}
    deriving (Show, Eq)

type Board = [Cell]

type TransformationOnCoords = (Int -> Int, Int -> Int)

frame :: Board -> Board
frame board = filter cellLives boardActedOn
    where
        allNeighbors = concat (flip neighbors board <$> board)
        boardAndNeighbors = uniq $ board ++ allNeighbors
        boardActedOn = fmap (flip actOnCell board) boardAndNeighbors
        uniq = foldr (\x acc -> if x `elem` acc then acc else x:acc) []

actOnCell :: Cell -> Board -> Cell
actOnCell cell board = case cell of
    (Cell True x y)  -> if livingNeighbors >= 2 && livingNeighbors <= 3 then (Cell True x y) else (Cell False x y)
    (Cell False x y) -> if livingNeighbors == 3 then (Cell True x y) else (Cell False x y)
    where
        livingNeighbors = length $ filter cellLives (neighbors cell board)

neighbors :: Cell -> Board -> [Cell]
neighbors cell board = [ upperRightNeighbor cell board, upperNeighbor cell board, upperLeftNeighbor cell board
                       , leftNeighbor cell board, rightNeighbor cell board
                       , lowerRightNeighbor cell board, lowerNeighbor cell board, lowerLeftNeighbor cell board ]

cellLives :: Cell -> Bool
cellLives (Cell True _ _) = True
cellLives _               = False

left :: Int -> Int
left x = x - 1

right :: Int -> Int
right x = x + 1

up :: Int -> Int
up y = y - 1

down :: Int -> Int
down y = y + 1

leftNeighbor :: Cell -> Board -> Cell
leftNeighbor = neighbor (id, left)

rightNeighbor :: Cell -> Board -> Cell
rightNeighbor = neighbor (id, right)

upperLeftNeighbor :: Cell -> Board -> Cell
upperLeftNeighbor = neighbor (up, left)

upperNeighbor :: Cell -> Board -> Cell
upperNeighbor = neighbor (up, id)

upperRightNeighbor :: Cell -> Board -> Cell
upperRightNeighbor = neighbor (up, right)

lowerLeftNeighbor :: Cell -> Board -> Cell
lowerLeftNeighbor = neighbor (down, left)

lowerNeighbor :: Cell -> Board -> Cell
lowerNeighbor = neighbor (down, id)

lowerRightNeighbor :: Cell -> Board -> Cell
lowerRightNeighbor = neighbor (down, right)

neighbor :: TransformationOnCoords -> Cell -> Board -> Cell
neighbor (g, f) (Cell s x y) = findCell (f x, g y)

findCell :: (Int, Int) -> Board -> Cell
findCell (x, y) board = case filter (\(Cell s x' y') -> x == x' && y == y') board of
                            [cell] -> cell
                            []     -> Cell False x y