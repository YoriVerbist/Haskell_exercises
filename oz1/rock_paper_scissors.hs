module Template where

data Move = Rock | Paper | Scissors
    deriving (Eq, Show)

beat :: Move -> Move
beat Rock = Paper
beat Scissors = Rock
beat Paper = Scissors

lose :: Move -> Move
lose Rock = Scissors
lose Scissors = Paper
lose Paper = Rock

data Result = Win | Lose | Draw
    deriving (Eq, Show)

outcome :: Move -> Move -> Result
outcome m1 m2
    | m1 == beat m2 = Win
    | m1 == lose m2 = Lose
    | otherwise     = Draw
