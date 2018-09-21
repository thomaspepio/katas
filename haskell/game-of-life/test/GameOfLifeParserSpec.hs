module GameOfLifeParserSpec where

import GameOfLifeParser
import GameOfLife

import Test.Hspec
import Text.Parsec

spec :: Spec
spec = describe "how to parse a text grid" $

    it "parses a textual format that represents a starting grid" $
        parseBoard "GRID: 1/2,2/1,2/3,3/1,3/3,4/2 END" `shouldBe` 
            Right [ Cell True 1 2, Cell True 2 1, Cell True 2 3, Cell True 3 1, Cell True 3 3, Cell True 4 2]