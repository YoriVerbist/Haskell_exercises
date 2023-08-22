module Template where

mySum :: [Integer] -> Integer
mySum [] = 0
mySum (x:xs)
    | null xs = x
    | otherwise = x + mySum xs

myProduct :: [Integer] -> Integer
myProduct [] = 1
myProduct (x:xs)
    | null xs = x
    | otherwise = x * myProduct xs

foldInts :: (Integer -> Integer -> Integer) -> Integer -> [Integer] -> Integer
foldInts f z []     = z
foldInts f z (x:xs) = f x $ foldInts f z xs

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f z []     = z
myFoldl f z (x:xs) = myFoldl f (f z x) xs 

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f z []     = z
myFoldr f z (x:xs) = f x $ myFoldr f z xs
