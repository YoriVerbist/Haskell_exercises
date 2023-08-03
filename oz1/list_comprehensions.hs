module Template where

factorial :: Integer -> Integer
factorial 0 = 1
factorial num = product [1..num]

myRepeat :: Int -> Int -> [Int]
myRepeat n x
    | n < 0 = []
    | otherwise = [x | _ <- [1..n]]

flatten :: [[Int]] -> [Int]
flatten l = [x | xs <- l, x <- xs]

range :: Int -> Int -> [Int]
range start end = [start..end]

sumInts :: Int -> Int -> Int
sumInts low high = sum [low..high]

removeMultiples :: Int -> [Int] -> [Int]
removeMultiples num l = [x | x <- l, x `mod` num /= 0]
