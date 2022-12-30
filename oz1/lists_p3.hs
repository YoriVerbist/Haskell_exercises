module Template where

-- * List Operations
-- ----------------------------------------------------------------------------

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = product [1..n]

myRepeat :: Int -> Int -> [Int]
myRepeat n x
    | n < 0 = []
    | otherwise = [x | _ <- [1..n]]

flatten :: [[Int]] -> [Int]
flatten = concat

range :: Int -> Int -> [Int]
range low high
    | low > high = []
    | otherwise = [low..high]

sumInts :: Int -> Int -> Int
sumInts low high
    | low > high = 0
    | otherwise = sum [low..high]

removeMultiples :: Int -> [Int] -> [Int]
removeMultiples mul l = [x | x <- l, x `mod` mul /= 0]


