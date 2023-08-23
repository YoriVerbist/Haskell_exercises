module Template where

import Data.List (delete)

-- * Selection Sort
-- ----------------------------------------------------------------------------

selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort [a] = [a]
selectionSort l = minimum l : selectionSort (delete (minimum l) l)

-- * Quicksort
-- ----------------------------------------------------------------------------

partitionFold :: (a -> Bool) -> [a] -> ([a],[a])
partitionFold f = foldr (\x (l1, l2) -> if f x then (x:l1, l2) else (l1, x:l2)) ([], [])

partitionFilter :: (a -> Bool) -> [a] -> ([a],[a])
partitionFilter p [] = ([], [])
partitionFilter p l  = (filter p l, filter (not . p) l)

partitionLC :: (a -> Bool) -> [a] -> ([a],[a])
partitionLC _ [] = ([], [])
partitionLC p l  = ([e | e <- l, p e], [e | e <- l, (not . p) e])

quicksort :: Ord a => [a] -> [a]
quicksort []  = []
quicksort [x] = [x]
quicksort (x:xs) = quicksort [l | l <- xs, l <= x]
                    ++
                    [x]
                    ++
                    quicksort [r | r <- xs, r > x]
