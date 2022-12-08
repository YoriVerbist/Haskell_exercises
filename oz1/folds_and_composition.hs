module Template where

foldr' :: (Int -> a -> a) -> a -> [Int] -> a
foldr' f z [] = z
foldr' f z (x :xs) = f x (foldr' f z xs)

length' :: [Int] -> Int
length' = foldr'(\x acc -> acc + 1) 0

any' :: (Int -> Bool) -> [Int] -> Bool
any' f = foldr' ((||) . f) False

all' :: (Int -> Bool) -> [Int] -> Bool
all' f = foldr' ((&&) . f) True

map' :: (Int -> Int) -> [Int] -> [Int]
map' f [] = []
map' f (x:xs) = f x: map f xs
