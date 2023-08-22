module Template where

applyAll :: [a -> a] -> a -> a
applyAll [] x = x
applyAll (f:fs) x 
    | null fs = f x
    | otherwise = f $ applyAll fs x

applyTimes :: Int -> (a -> a) -> a -> a
applyTimes amount f x
    | amount < 1 = x
    | otherwise = applyTimes (amount - 1) f . f $ x

applyMultipleFuncs :: a -> [a -> b] -> [b]
applyMultipleFuncs _ [] = []
applyMultipleFuncs x (f:fs)
    | null fs = [f x]
    | otherwise = [f x] ++ applyMultipleFuncs x fs
