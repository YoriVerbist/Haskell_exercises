module Template where

double :: Int -> Int
double x = 2 * x

myAbs :: Int -> Int
myAbs x
    | x < 0 = -x
    | otherwise = x

toFahrenheit :: Float -> Float
toFahrenheit c = 1.8 * c + 32

fizzbuzz :: Int -> String
fizzbuzz x
    | mod x 5 == 0 && mod x 3 == 0 = "fizzbuzz"
    | mod x 5 == 0 = "buzz"
    | mod x 3 == 0 = "fizz"
    | otherwise = show x
