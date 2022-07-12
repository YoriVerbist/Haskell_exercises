module Template where

even' :: Int -> Bool
even' x = x `mod` 2 == 0

not' :: Bool -> Bool
not' x
    | x = False
    | otherwise = True

absolute' :: Int -> Int
absolute' x
    | x < 0 = -x
    | otherwise = x

greatherThanFive :: Int -> Bool
greatherThanFive = (>5)

amountEven :: [Int] -> Int
amountEven = foldr (\x acc -> if even' x then acc + 1 else acc) 0 

onlyOdd :: [Int] -> [Int]
onlyOdd = foldr (\x acc -> if not $ even' x then x:acc else acc) []

absGtFive :: [Int] -> Int
absGtFive = foldr (\x acc -> if even' x && greatherThanFive (absolute' x) then acc + 1 else acc) 0

anyEvenGtFive :: [Int] -> Bool
anyEvenGtFive = foldr (\x acc -> even' x && greatherThanFive x || acc) False


