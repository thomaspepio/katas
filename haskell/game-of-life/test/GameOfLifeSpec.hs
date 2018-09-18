module GameOfLifeSpec where

import GameOfLife    
import Test.Hspec
    
spec :: Spec
spec = describe "The Game of Life" $ do

    let board = [[Cell True 0 0, Cell True 1 0, Cell True 2 0, Cell True 3 0, Cell False 4 0]
                ,[Cell False 0 1, Cell False 1 1, Cell False 2 1, Cell True 3 1, Cell False 4 1]
                ,[Cell False 0 2, Cell False 1 2, Cell False 2 2, Cell False 3 2, Cell True 4 2]
                ,[Cell True 0 3, Cell True 1 3, Cell True 2 3, Cell False 3 3, Cell False 4 3]
                ,[Cell True 0 4, Cell True 1 4, Cell False 2 4, Cell False 3 4, Cell False 4 4]]

    describe "accessing neihbors of a cell" $ do
        it "finds the left neighbor" $
            leftNeighbor (Cell False 1 1) board `shouldBe` Just (Cell False 0 1)
        it "finds the right neighbor" $
            rightNeighbor (Cell False 1 1) board `shouldBe` Just (Cell False 2 1)
        it "finds the upper left neighbor" $
            upperLeftNeighbor (Cell False 1 1) board `shouldBe` Just (Cell True 0 0)
        it "finds the upper neighbor" $
            upperNeighbor (Cell False 1 1) board `shouldBe` Just (Cell True 1 0)
        it "finds the upper right neighbor" $
            upperRightNeighbor (Cell False 1 1) board `shouldBe` Just (Cell True 2 0)
        it "finds the lower left neighbor" $
            lowerLeftNeighbor (Cell False 1 1) board `shouldBe` Just (Cell False 0 2)
        it "finds the lower neighbor" $
            lowerNeighbor (Cell False 1 1) board `shouldBe` Just (Cell False 1 2)
        it "finds the lower right neighbor" $
            lowerRightNeighbor (Cell False 1 1) board `shouldBe` Just (Cell False 2 2)

    describe "wrong accesses to neighbors" $ do
        it "any leftmost cell has no left neighbor" $
            leftNeighbor (Cell False 0 1) board `shouldBe` Nothing
        it "any rightmost cell has no right neighbor" $
            rightNeighbor (Cell True 4 1) board `shouldBe` Nothing
        it "the first cell has no upper left neighbor" $
            upperLeftNeighbor (Cell True 0 0) board `shouldBe` Nothing
        it "the last cell of the first row has no upper right neighbor" $
            upperRightNeighbor (Cell True 2 0) board `shouldBe` Nothing
        it "the first cell of the last row has no lower left neighbor" $
            lowerLeftNeighbor (Cell False 0 2) board `shouldBe` Nothing
        it "the last cell of the last row has no lower right neighbor" $
            lowerRightNeighbor (Cell False 3 4) board `shouldBe` Nothing
        
    describe "rules for living and dying cells" $ do
        it "a dead cell with three living neighbors becomes alive" $
            actOnDeadCell (Cell False 1 1) board `shouldBe` (Cell True 1 1)
        it "a living cell that has less than two alive neighbors dies" $
            actOnLivingCell (Cell True 4 2) board `shouldBe` (Cell False 4 2)
        it "a living cell that has more than three alive neighbors dies" $
            actOnLivingCell (Cell True 1 4) board `shouldBe` (Cell False 1 4)
