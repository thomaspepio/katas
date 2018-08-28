{-# LANGUAGE OverloadedStrings #-}
module RomanNumerals(intToRoman) where

import           Control.Monad.Writer
import           Data.HashMap hiding (filter)
import           Data.Maybe
import qualified Data.Text as DT
import           Prelude hiding (lookup)
import           System.Exit

type RomanNumeral = DT.Text

romanNumerals :: Map Int RomanNumeral
romanNumerals = fromList [(1, "I"), (4, "IV"), (5, "V"), (9, "IX"),
                          (10, "X"), (49, "IL"), (50, "L"), (90, "XC"),
                          (100, "C"), (400, "CD"), (500, "D"), (900, "CM"),
                          (1000, "M")]

intToRoman :: Int -> RomanNumeral
intToRoman 0 = ""
intToRoman x = snd $ runWriter (toRoman x)

toRoman :: Int -> Writer RomanNumeral Int
toRoman 0 = return 0
toRoman n = do
    let toSubstract = maximum $ filter (\x -> n - x >= 0) (keys romanNumerals)
    let numeral = lookup toSubstract romanNumerals
    if toSubstract == 0 
        then do 
            tell (fromMaybe "" numeral)
            return n 
        else do
            tell (fromMaybe "" numeral)
            toRoman (n - toSubstract)
