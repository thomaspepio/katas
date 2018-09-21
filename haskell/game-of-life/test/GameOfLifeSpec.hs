module GameOfLifeSpec where

import GameOfLife    
import Test.Hspec
    
spec :: Spec
spec = describe "The Game of Life" $ do

    let board = [ Cell True 0 0, Cell True 1 0, Cell True 2 0, Cell True 3 0
                , Cell True 3 1, Cell True 4 2, Cell True 0 3, Cell True 1 3
                , Cell True 2 3, Cell True 0 4, Cell True 1 4 ]

    describe "accessing neihbors of a cell" $ do
        it "finds the left neighbor" $
            leftNeighbor (Cell False 1 1) board `shouldBe` Cell False 0 1
        it "finds the right neighbor" $
            rightNeighbor (Cell False 1 1) board `shouldBe` Cell False 2 1
        it "finds the upper left neighbor" $
            upperLeftNeighbor (Cell False 1 1) board `shouldBe` Cell True 0 0
        it "finds the upper neighbor" $
            upperNeighbor (Cell False 1 1) board `shouldBe` Cell True 1 0
        it "finds the upper right neighbor" $
            upperRightNeighbor (Cell False 1 1) board `shouldBe` Cell True 2 0
        it "finds the lower left neighbor" $
            lowerLeftNeighbor (Cell False 1 1) board `shouldBe` Cell False 0 2
        it "finds the lower neighbor" $
            lowerNeighbor (Cell False 1 1) board `shouldBe` Cell False 1 2
        it "finds the lower right neighbor" $
            lowerRightNeighbor (Cell False 1 1) board `shouldBe` Cell False 2 2

    describe "rules for living and dying cells" $ do
        it "a dead cell with three living neighbors becomes alive" $
            actOnCell (Cell False 1 1) board `shouldBe` (Cell True 1 1)
        it "a living cell that has less than two alive neighbors dies" $
            actOnCell (Cell True 4 2) board `shouldBe` (Cell False 4 2)
        it "a living cell that has more than three alive neighbors dies" $
            actOnCell (Cell True 1 4) board `shouldBe` (Cell False 1 4)
        

    describe "transitions on boards" $
        describe "computes oscilating figure(s)" $ do

            let blinking = [ Cell True 0 1, Cell True 1 1, Cell True 2 1 ]
            it "the blinker" $
                frame blinking `shouldMatchList` [ Cell True 1 0, Cell True 1 1, Cell True 1 2 ]

            let square = [ Cell True 1 1, Cell True 2 1, Cell True 1 2, Cell True 2 2 ]
            it "the block" $
                frame square `shouldMatchList` square

            let beehive = [ Cell True 1 2, Cell True 2 1, Cell True 2 3, Cell True 3 1, Cell True 3 3, Cell True 4 2]
            it "the beehive" $
                frame beehive `shouldMatchList` beehive
            
            let tub = [ Cell True 1 2, Cell True 2 1, Cell True 2 3, Cell True 3 2 ]
            it "the tub" $
                frame tub `shouldMatchList` tub
