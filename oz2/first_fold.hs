module Template where

foldr' :: (Int -> a -> a) -> a -> [Int] -> a
foldr' f z []       = z
foldr' f z (x : xs) = f x (foldr f z xs)

length' :: [Int] -> Int
length' = foldr' (\x -> (+) 1) 0

any' :: (Int -> Bool) -> [Int] -> Bool
any' f = foldr' (\x acc -> f x || acc) False

all' :: (Int -> Bool) -> [Int] -> Bool
all' f = foldr' (\x acc -> f x && acc) True

map' :: (Int -> Int) -> [Int] -> [Int]
map' f = foldr' ((:) . f) []

filter' :: (Int -> Bool) -> [Int] -> [Int]
filter' f = foldr' (\x acc -> if f x then x: acc else acc) []

