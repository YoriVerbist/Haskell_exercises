module MyHaskell where

import Data.List

-- 1.1
data LineItem = Space | Newline | Word String
    deriving (Eq
              -- REMOVE Show after implementing 1.3
              --, Show
             )

-- 1.2
mkSpace :: LineItem
mkSpace = Space

mkNewline :: LineItem
mkNewline = Newline 

mkWord :: String -> LineItem
mkWord w = Word w

-- 1.3
lineItemToStr :: LineItem -> String
lineItemToStr Space    = show " "
lineItemToStr Newline  = show "\n"
lineItemToStr (Word w) = show w

-- Comment out and complete this code
instance Show LineItem where
  show = lineItemToStr

-- 1.4
toLineItems :: String -> [LineItem]
toLineItems l 
    | null l = []
    | head l == ' ' = mkSpace : toLineItems (tail l)
    | head l == '\n' = mkNewline : toLineItems  (tail l)
    | otherwise = mkWord (findWord l) : toLineItems (dropWord l)
        where
            findWord = takeWhile (\x -> x /= ' ') 
            dropWord = dropWhile (\x -> x /= ' ')

-- 1.5
fromLineItems :: [LineItem] -> String
fromLineItems = error "Not implemented"


-- 2.1
removeSpaces :: [LineItem] -> [LineItem]
removeSpaces = error "Not implemented"

-- 2.2
splitInLines :: [LineItem] -> [[LineItem]]
splitInLines = error "Not implemented"

-- 2.3
separateTooLongWords :: Int -> [LineItem] -> [[LineItem]]
separateTooLongWords = error "Not implemented"

-- 2.4
wrap :: Int -> [LineItem] -> [[LineItem]]
wrap = error "Not implemented"

-- 2.5
joinLineWithSpaces :: [LineItem] -> [LineItem]
joinLineWithSpaces = error "Not implemented"

-- 2.6
joinLinesWithNewlines :: [[LineItem]] -> [LineItem]
joinLinesWithNewlines = error "Not implemented"

-- DO NOT CHANGE THIS FUNCTION
wordWrap :: Int -> String -> String
wordWrap lineWidth =
    fromLineItems .
    joinLinesWithNewlines .
    map joinLinesWithNewlines .
    map (map joinLineWithSpaces . concatMap (wrap lineWidth)) .
    map (separateTooLongWords lineWidth) .
    splitInLines .
    removeSpaces .
    toLineItems


-- 3.1
getLines :: IO String
getLines = error "Not implemented"

-- 3.2
interactiveWrapper :: IO ()
interactiveWrapper = error "Not implemented"

