{-# LANGUAGE OverloadedStrings #-}
module Interpreter
    (
        interpret,
        Command(..)
    )
where

import           Data.Text
import           Game
import           System.Exit
import           Text.Read

data Command = End | Unknown deriving (Eq, Show)

interpret :: Text -> Either Command Roll
interpret "x"   = Right Strike
interpret "/"   = Right Spare
interpret "."   = Right Miss
interpret "END" = Left End
interpret s = case read of
    Just i  -> Right  (Reg i)
    Nothing -> Left Unknown
    where
        read = readMaybe (unpack s)
