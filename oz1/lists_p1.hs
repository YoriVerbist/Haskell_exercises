module Template where

count :: [Int] -> Int
count list
    | null list = 0
    | otherwise = 1 + count (tail list)

myAnd :: [Bool] -> Bool
myAnd list
    | null list = True
    | not (head list) = False
    | otherwise = myAnd (tail list)

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs)
    | x = True
    | otherwise = myOr xs

append :: [Int] -> [Int] -> [Int]
append l1 l2
    | null l1 = l2
    | null l2 = l1
    | otherwise = l1 ++ l2
