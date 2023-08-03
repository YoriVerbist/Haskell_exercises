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

map' :: (Int -> Int) -> [Int] -> [Int]
map' f l = foldr' (\x acc -> f x : acc) [] l

filter' :: [Int] -> (Int -> Bool) -> [Int]
filter' l f = foldr' (\x acc -> if f x then x:acc else acc) [] l

-- * Given helpers

even' :: Int -> Bool
even' = even

not' :: Bool -> Bool
not' = not

absolute' :: Int -> Int
absolute' = abs

greaterThanFive :: Int -> Bool
greaterThanFive x = x > 5

-- * Beginning Composer

amountEven :: [Int] -> Int
amountEven l = sum $ filter' l even

onlyOdd :: [Int] -> [Int]
onlyOdd l = filter' l (not . even')

absGtFive :: [Int] -> Int
absGtFive l = length' $ filter' (map absolute' l) greaterThanFive

anyEvenGtFive :: [Int] -> Bool
anyEvenGtFive l = any' greaterThanFive (filter' l even')

anyEvenGtFive' :: [Int] -> Bool
anyEvenGtFive' l  
    | (length' $ filter' (filter' l even') greaterThanFive) > 0 = True
    | otherwise = False

