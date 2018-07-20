module Game
    (
        score, scoreThrow,
        Throw(..), Game(..)
    )
where

data Throw = Strike | Spare | Reg Int | Miss deriving (Eq, Show)
newtype Game = Game [Throw]

score :: Game -> Int
score (Game rs) = case rs of
    (Strike:Strike:[Strike]) -> 30
    (Strike:x:[y])           -> scoreThrow Strike + scoreThrow x + scoreThrow y
    (Reg i:Spare:[Reg j])    -> scoreThrow Spare + scoreThrow (Reg j)
    (Strike:x:y:zs)          -> scoreThrow Strike + scoreThrow x + scoreThrow y + score (Game $ x:y:zs)
    (x:Spare:y:zs)           -> scoreThrow Spare + scoreThrow y + score (Game $ y:zs)
    (x:ys)                   -> scoreThrow x + score (Game ys)
    []                       -> 0

scoreThrow :: Throw -> Int
scoreThrow Miss    = 0
scoreThrow (Reg n) = n
scoreThrow Spare   = 10
scoreThrow Strike  = 10
