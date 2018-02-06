module Game
    (
        score, score',
        Roll(..), Game(..)
    )
where

data Roll = Strike | Spare | Reg Int | Miss deriving (Eq, Show)
newtype Game = Game [Roll]

score :: Game -> Int
score (Game rs) = case rs of
    (Strike:Strike:[Strike]) -> 30 -- last frame for a whole strike game
    (Reg i:Spare:[Reg j])    -> 15 -- last frame for a whole spare game
    (Strike:x:y:zs)          -> 10 + score' x + score' y + score (Game $ x:y:zs)
    (x:Spare:y:zs)           -> 10 + score' y + score (Game $ y:zs)
    (x:ys)                   -> score' x + score (Game ys)
    []                       -> 0

score' :: Roll -> Int
score' Miss    = 0
score' (Reg n) = n
score' Spare   = 10
score' Strike  = 10
