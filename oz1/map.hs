module Template where

myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = f x : myMap f xs

myMapF :: (a -> b) -> [a] -> [b]
myMapF f = foldr (\x acc -> f x : acc) []
