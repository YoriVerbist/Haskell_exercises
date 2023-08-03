module Template where

myProduct :: [Integer] -> Integer
myProduct = foldr (*) 1

insert :: Int -> [Int] -> [Int]
insert n [] = [n]
insert n (x:xs)
    | n <= x = n:x:xs
    | otherwise = x : insert n xs

myLast :: [Int] -> Int
myLast l = foldl (\_ x -> x) 0 l
