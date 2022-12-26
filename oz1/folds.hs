module Template where

mySum :: [Integer] -> Integer
mySum list
    | null list = 0
    | otherwise = head list + mySum (tail list)

myProduct :: [Integer] -> Integer
myProduct list
    | null list = 1
    | otherwise = head list * myProduct (tail list)

foldInts :: (Integer -> Integer -> Integer) -> Integer -> [Integer] -> Integer
foldInts fn base [] = base
foldInts fn base ints = fn (head ints) (foldInts fn base (tail ints))

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl fn base [] = base
myFoldl fn base list = myFoldl fn (fn base (head list)) (tail list)

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr fn base [] = base
myFoldr fn base list = fn (head list) (myFoldr fn base (tail list))

readInBase :: Int -> [Int] -> Int
readInBase base = foldl (\acc x -> acc * base + x) 0

myMap :: (a -> b) -> [a] -> [b]
myMap fn [] = []
myMap fn (x:xs)
    | null xs = [fn x]
    | otherwise = fn x : myMap fn xs

myMapF :: (a -> b) -> [a] -> [b]
myMapF fn = foldr (\x acc -> fn x:acc) []


