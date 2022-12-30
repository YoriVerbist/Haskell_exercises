module Template where

import Data.Char (isAlphaNum, toUpper, toLower)

type Username = String
type Domain = String

data Email = MkEmail Username Domain
  deriving (Eq, Show)

data Server = MkServer Domain [Email]
  deriving (Eq, Show)

data Report = MkReport Domain [Email]
  deriving (Eq, Show)

externalSenders :: [Server] -> [Report]
externalSenders = map (\(MkServer domain emails) -> MkReport domain (filter (\(MkEmail _ dom) -> dom /= domain) emails))

unique :: [Integer] -> [Integer]
unique [] = []
unique (x:xs) = x:unique (filter (/= x) xs)

type Letter = Char
type Author = String

data Shelf = MkShelf Letter [Author]
  deriving (Eq, Show)

fixShelves :: [Shelf] -> [Shelf]
fixShelves = filter (not . null . (\(MkShelf _ authors) -> authors)) . map (\(MkShelf letter authors) -> MkShelf letter $ filter ((/=) letter . head) authors)

elimContainsChar :: Char -> [String] -> [String]
elimContainsChar l = filter (\s -> l `notElem` s)

leetSpeak :: [String] -> [String]
leetSpeak = map (map repl)
    where
        repl 'a' = '4'
        repl 'e' = '3'
        repl 'i' = '1'
        repl 'o' = '0'
        repl 't' = '7'
        repl 'A' = '4'
        repl 'E' = '3'
        repl 'I' = '1'
        repl 'O' = '0'
        repl 'T' = '7'
        repl c = c

isPalindrome :: String -> Bool
isPalindrome l = foldr (\x ys -> ys ++ [x]) [] temp == temp
    where
        temp = map toLower $ filter isAlphaNum l


type Species = String
type Length  = Float     -- in inches

data Fish = MkFish Species Length
  deriving (Eq, Show)

viableFish :: [Fish] -> [String] -> [Fish]
viableFish fish invasive= filter (\(MkFish name len) -> name `notElem` invasive && len > 8) fish
