module Template where

myProduct :: [Integer] -> Integer
myProduct = foldr (*) 1

insert ::Int -> [Int] -> [Int]
insert num list
    | null list = [num]
    | head list > num = num:list
    | maximum list < num = list ++ [num]
    | otherwise = [head list] ++ insert num (tail list)

myLast :: [Int] -> Int
myLast [] = error "no empty lists allowed"
myLast (x:[]) = x
myLast (x:xs) = myLast xs
