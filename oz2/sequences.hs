module Template where

import Data.Char

class Sequence a where
    next :: a -> a
    prev :: a -> a

instance Sequence Integer where
    next x = x + 1
    prev x = x - 1

instance Sequence Char where
    next a = chr . (+1) . ord $ a
    prev a = chr . (\x -> x - 1) . ord $ a

instance Sequence Bool where
    next = not
    prev = not

class Sequence a => LeftBoundedSequence a where
    firstElem :: a

class Sequence a => RightBoundedSequence a where
    lastElem :: a

instance LeftBoundedSequence Char where
    firstElem = 'a'

instance LeftBoundedSequence Bool where
    firstElem = True

instance RightBoundedSequence Char where
    lastElem = 'z'

instance RightBoundedSequence Bool where
    lastElem = True
