module Template where

foldr' :: (Int -> a -> a) -> a -> [Int] -> a
foldr' f z [] = z
foldr' f z (x : xs) = f x (foldr f z xs)

length' :: [Int] -> Int
length' l = foldr' (\_ -> (+ 1)) 0 l

any' :: (Int -> Bool) -> [Int] -> Bool
any' f l = foldr' (\x acc -> (f x) || acc) False l

all' :: (Int -> Bool) -> [Int] -> Bool
all' f l = foldr' (\x acc -> (f x) && acc) True l
