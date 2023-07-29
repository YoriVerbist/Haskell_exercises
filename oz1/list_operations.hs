module Template where

myProduct :: [Integer] -> Integer
myProduct []     = 1
myProduct (x:xs) = x * myProduct xs

insert :: Int -> [Int] -> [Int]
insert num [] = [num]
insert num (x:xs) 
    | num <= x = num:x:xs
    | otherwise = xs  
