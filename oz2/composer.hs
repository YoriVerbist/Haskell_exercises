module Template where

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
amountEven = length . filter even'

onlyOdd :: [Int] -> [Int]
onlyOdd = filter (not' . even')

absGtFive :: [Int] -> Int
absGtFive = length . filter (greaterThanFive . absolute')

anyEvenGtFive :: [Int] -> Bool
anyEvenGtFive = any greaterThanFive . filter even'

anyEvenGtFive' :: [Int] -> Bool
anyEvenGtFive' = any greaterThanFive . filter even'
