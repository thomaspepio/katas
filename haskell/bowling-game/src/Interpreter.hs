{-# LANGUAGE OverloadedStrings #-}
module Interpreter
    (
        makeGame, interpret
    )
where

import           Data.Text
import           Game
import           System.Exit
import           Text.Read

makeGame :: [Roll] -> Game
makeGame _ = undefined

makeRolls :: Text -> Text -> [Roll]
makeRolls _ _  = undefined
-- makeRolls "" _ = []
-- makeRolls i "" = []
-- makeRolls i c  = interpret <$> splitOn c i

interpret :: Text -> Maybe Roll
interpret "x" = Just Strike
interpret "/" = Just Spare
interpret "." = Just Miss
interpret s = case read of
    Just i  -> Just (Reg i)
    Nothing -> Nothing
    where
        read = readMaybe (unpack s)
