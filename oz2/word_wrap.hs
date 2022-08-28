module MyHaskell where

-- 1.1
data LineItem = Space | Newline | Word String
    deriving (Eq)

-- 1.2
mkSpace :: LineItem
mkSpace = Space

mkNewline :: LineItem
mkNewline = Newline

mkWord :: String -> LineItem
mkWord = Word

-- 1.3
instance Show LineItem where
  show Space = show " "
  show Newline = show "\n"
  show (Word s) = show s

-- 1.4
toLineItems :: String -> [LineItem]
toLineItems s
    | "" <- before = continue after
    where
        (before, after) = span (`notElem` " \n") s
        continue "" = []

-- 1.5
fromLineItems :: [LineItem] -> String
fromLineItems = concatMap fromItem
    where 
        fromItem Space = " "
        fromItem Newline = "\n"
        fromItem s = show s


-- 2.1
removeSpaces :: [LineItem] -> [LineItem]
removeSpaces items = [item | item <- items, item /= Space]

-- 2.2
splitInLines :: [LineItem] -> [[LineItem]]
splitInLines [] = [[]]
splitInLines items = before: after'
    where 
        (before, after) = break (== Newline) items
        after'
            | _:is' <- after = splitInLines is'
            | otherwise = []

-- 2.3
separateTooLongWords :: Int -> [LineItem] -> [[LineItem]]
separateTooLongWords len items 
    | [] <- before = tail
    | otherwise = before:tail
    where
        wordlen (Word w) = length w
        wordlen _ = 0
        (before, after) = break (== Newline) items
        tail
            | tooLong:after' <- after = [tooLong] : separateTooLongWords len after'
            | otherwise = []
        

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

