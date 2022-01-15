module Template where

factorial :: Integer -> Integer
factorial n
    | n <= 0 = 1
    | otherwise = product [1..n]

myRepeat :: Int -> Int -> [Int]
myRepeat n x
    | n <= 0 = []
    | otherwise = take n (cycle[x])

flatten :: [[Int]] -> [Int]
flatten l = [y | x <- l, y <- x]

range :: Int -> Int -> [Int]
range min max
    | min > max = []
    | otherwise = [min..max]

sumInts :: Int -> Int -> Int
sumInts low high
    | low > high = 0
    | otherwise = sum [low..high]

removeMultiples :: Int -> [Int] -> [Int]
removeMultiples n list
    | null list = []
    | otherwise = [x | x <- list, x `mod` n /= 0]
