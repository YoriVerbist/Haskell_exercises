module Template where

data Move = Rock|Paper|Scissors
    deriving (Eq, Show)

beat :: Move -> Move
beat m
    | m == Rock = Paper
    | m == Paper = Scissors
    | m == Scissors = Rock

lose :: Move -> Move
lose m
    | m == Rock = Scissors
    | m == Paper = Rock
    | m == Scissors = Paper

data Result = Win|Lose|Draw
  deriving (Eq, Show)

outcome :: Move -> Move -> Result
outcome m1 m2
    | m1 == Paper && m2 == Paper = Draw
    | m1 == Rock && m2 == Rock = Draw
    | m1 == Scissors && m2 == Scissors = Draw
    | m1 == Paper && m2 == Rock = Win
    | m2 == Paper && m1 == Rock = Lose
    | m1 == Paper && m2 == Scissors = Lose
    | m2 == Paper && m1 == Scissors = Win
    | m1 == Rock && m2 == Scissors = Win
    | m2 == Rock && m1 == Scissors = Win
