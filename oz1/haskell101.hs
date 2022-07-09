module Template where

double :: Int -> Int
double x = x * 2

myAbs :: Int -> Int
myAbs x 
    | x < 0 = -x
    | otherwise = x

toFahrenheit :: Float -> Float
toFahrenheit x = 1.8 * x + 32

fizzbuzz :: Int -> String
fizzbuzz x
    | (mod x 3 == 0) && (mod x 5 == 0) = "fizzbuzz"
    | (mod x 3 /= 0) && (mod x 5 == 0) = "buzz"
    | (mod x 3 == 0) && (mod x 5 /= 0) = "fizz"
    | otherwise = show x
