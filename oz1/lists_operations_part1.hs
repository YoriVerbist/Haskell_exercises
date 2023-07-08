module Template where

count :: [Int] -> Int
count []     = 0
count (x:xs) = 1 + count xs

myAnd :: [Bool] -> Bool
myAnd []     = True
myAnd (x:xs) = x && myAnd xs

myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myAnd xs

append :: [Int] -> [Int] -> [Int]
append l1 l2 = l1 ++ l2
