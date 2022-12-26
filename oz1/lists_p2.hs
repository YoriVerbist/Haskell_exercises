module Template where

myProduct :: [Integer] -> Integer
myProduct = product

insert :: Int -> [Int] -> [Int]
insert a [] = [a]
insert a (x:xs)
    | a <= x = a:x:xs
    | otherwise = x: insert a xs

myLast :: [Int] -> Int
myLast (x:xs)
    | null xs = x
    | otherwise = myLast xs
