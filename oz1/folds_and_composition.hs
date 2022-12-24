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
map' f = foldr' ((:) . f) []

filter' :: [Int] -> (Int -> Bool) -> [Int]
filter' l f = foldr' (\x acc -> if f x then x: acc else acc) []

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
amountEven = length . filter even

onlyOdd :: [Int] -> [Int]
onlyOdd = filter odd

absGtFive :: [Int] -> Int
absGtFive = length . filter (greaterThanFive . abs)

anyEvenGtFive :: [Int] -> Bool
anyEvenGtFive = any greaterThanFive . filter even

anyEvenGtFive' :: [Int] -> Bool
anyEvenGtFive' = any greaterThanFive . filter even

