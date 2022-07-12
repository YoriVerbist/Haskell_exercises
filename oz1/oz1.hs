module Template where

-- List Operations

myProduct :: [Integer] -> Integer
myProduct = foldr (*) 1

insert :: Int -> [Int] -> [Int]
insert x l = x:l

myLast :: [Int] -> Int
myLast = foldl (\acc x -> x) 0

-- Start Rock - Paper - Scissors
data Move = Rock | Paper | Scissors deriving (Eq, Show)
data Result = Win | Lose | Draw deriving(Eq, Show)

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

outcome :: Move -> Move -> Result
outcome m1 m2
    | m1 == beat m2 = Win
    | m1 == m2 = Draw
    | otherwise = Lose

-- List Comprehensions
factorial :: Integer -> Integer
factorial x 
    | x > 0 = product [1..x]
    | otherwise = 1

myRepeat :: Int -> Int -> [Int]
myRepeat n x
    | n < 0 = []
    | otherwise = [x | _ <- [1..n]]

flatten :: [[Int]] -> [Int]
flatten = concat

range :: Int -> Int -> [Int]
range x y
    | x < y = [x..y]
    | x == y = [x]
    | otherwise = []

sumInts :: Int -> Int -> Int
sumInts low high
    | low > high = 0
    | otherwise = sum [low..high]

removeMultiples :: Int -> [Int] -> [Int]
removeMultiples mul l = [x | x <- l, x `mod` mul /= 0]

mapLC :: (a -> b) -> [a] -> [b]
mapLC f l = [f x | x <- l]

filterLC :: (a -> Bool) -> [a] -> [a]
filterLC f l = [x | x <- l, f x]

mySum :: [Integer] -> Integer
mySum = foldr (+) 0

foldInts :: (Integer -> Integer -> Integer) -> Integer -> [Integer] -> Integer
foldInts f z = foldr f z

