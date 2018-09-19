module GameOfLife
    ( 
        Cell(..), Board(..),
        frame,
        actOnDeadCell, actOnLivingCell,
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

type Board = [[Cell]]

type TransformationOnCoords = (Int -> Int, Int -> Int)

fakeCell :: Cell
fakeCell = Cell False 0 0

frame :: Board -> Board
frame board = fmap (fmap (\cell -> mutate cell board)) board

mutate :: Cell -> Board -> Cell
mutate cell board = case cell of
    (Cell True _ _)  -> actOnLivingCell cell board
    (Cell False _ _) -> actOnDeadCell cell board

actOnDeadCell :: Cell -> Board -> Cell
actOnDeadCell = actOnCell (\x -> length x == 3)

actOnLivingCell :: Cell -> Board -> Cell
actOnLivingCell = actOnCell (\x -> length x >= 2 && length x <= 3)

actOnCell :: ([Cell] -> Bool) -> Cell -> Board -> Cell
actOnCell condition (Cell s x y) board =
    let
        cell = (Cell s x y)
        neighbors = [fromMaybe fakeCell (upperRightNeighbor cell board)
                   , fromMaybe fakeCell (upperNeighbor cell board)
                   , fromMaybe fakeCell (upperLeftNeighbor cell board)
                   , fromMaybe fakeCell (leftNeighbor cell board)
                   , fromMaybe fakeCell (rightNeighbor cell board)
                   , fromMaybe fakeCell (lowerRightNeighbor cell board)
                   , fromMaybe fakeCell (lowerNeighbor cell board)
                   , fromMaybe fakeCell (lowerLeftNeighbor cell board)]
        aliveNeighbors = filter (\(Cell s _ _) -> s) neighbors
    in
        if condition aliveNeighbors then (Cell True x y) else (Cell False x y)

left :: Int -> Int
left x = x - 1

right :: Int -> Int
right x = x + 1

up :: Int -> Int
up y = y - 1

down :: Int -> Int
down y = y + 1

leftNeighbor :: Cell -> Board -> Maybe Cell
leftNeighbor = neighbor (id, left)

rightNeighbor :: Cell -> Board -> Maybe Cell
rightNeighbor = neighbor (id, right)

upperLeftNeighbor :: Cell -> Board -> Maybe Cell
upperLeftNeighbor = neighbor (up, left)

upperNeighbor :: Cell -> Board -> Maybe Cell
upperNeighbor = neighbor (up, id)

upperRightNeighbor :: Cell -> Board -> Maybe Cell
upperRightNeighbor = neighbor (up, right)

lowerLeftNeighbor :: Cell -> Board -> Maybe Cell
lowerLeftNeighbor = neighbor (down, left)

lowerNeighbor :: Cell -> Board -> Maybe Cell
lowerNeighbor = neighbor (down, id)

lowerRightNeighbor :: Cell -> Board -> Maybe Cell
lowerRightNeighbor = neighbor (down, right)

neighbor :: TransformationOnCoords -> Cell -> Board -> Maybe Cell
neighbor (g, f) (Cell s x y) board = do
    let newX = f x
    let newY = g y
    case (newX < 0 || newY < 0) || (newX >= length board || newY >= length board) of
        True  -> Nothing
        False -> do
            let candidateCell = board !! newY !! newX
            case candidateCell == (Cell s x y) of
                True  -> Nothing
                False -> Just candidateCell