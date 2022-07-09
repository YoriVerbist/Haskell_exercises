module Template where

count :: [Int] -> Int
count [] = 0
count (x:xs) = 1 + count xs

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs)
    | not x = False
    | otherwise = myAnd xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs)
    | x = True
    | otherwise = myOr xs

append :: [Int] -> [Int] -> [Int]
append list [] = list
append [] list = list
