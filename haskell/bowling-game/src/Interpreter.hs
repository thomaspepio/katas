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

data Command = Move Roll | End | Unknown deriving (Eq, Show)

interpret :: Text -> Command
interpret "x"   = Move Strike
interpret "/"   = Move Spare
interpret "."   = Move Miss
interpret "END" = End
interpret s = case read of
    Just i  -> Move (Reg i)
    Nothing -> Unknown
    where
        read = readMaybe (unpack s)
