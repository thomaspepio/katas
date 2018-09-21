module GameOfLifeParser
    (
        parseBoard
    )
where

import GameOfLife

import Control.Monad.Identity
import Text.Parsec

type Parser a = ParsecT String () Identity a

parseBoard :: String -> Either ParseError Board
parseBoard s = parse board "" s

board :: Parser Board
board = do
    string "GRID:"
    spaces
    cells <- cellList
    spaces
    string "END"
    return cells

cellList :: Parser [Cell]
cellList = cell `sepBy1` comma

cell :: Parser Cell
cell = do
    x <- number
    char '/'
    y <- number
    return $ Cell True (read x) (read y)

comma :: Parser String
comma = string ","

number :: Parser String
number = many digit