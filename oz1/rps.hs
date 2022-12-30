module Template where

-- * Rock - Paper - Scissors
-- ----------------------------------------------------------------------------

data Move = Rock | Paper | Scissors
  deriving (Eq, Show)

beat :: Move -> Move
beat m 
    | m == Rock = Paper
    | m == Paper = Scissors
    | otherwise = Rock

lose :: Move -> Move
lose m
    | m == Rock = Scissors
    | m == Paper = Rock
    | otherwise = Paper

data Result = Win | Lose | Draw
  deriving (Eq, Show)

outcome :: Move -> Move -> Result
outcome m1 m2
    | m1 == beat m2 = Win
    | m1 == m2 = Draw
    | otherwise = Lose
