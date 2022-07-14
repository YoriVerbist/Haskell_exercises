module Template where
import Data.Char

class Sequence a where
    next :: a -> a 
    prev :: a -> a

instance Sequence Integer where
    next x = x + 1
    prev x = x - 1

instance Sequence Char where
    next x = chr $ ord x + 1
    prev x = chr $ ord x - 1

instance Sequence Bool where
    next True = False
    next False = True 
    prev True = False
    prev False = True 

class Sequence a => LeftBoundedSequence a where
    firstElem :: a

class Sequence a => RightBoundedSequence a where
    lastElem ::  a

instance LeftBoundedSequence Char where
    firstElem = 'a'

instance LeftBoundedSequence Bool where
    firstElem = False

instance RightBoundedSequence Char where
    lastElem = 'z'

instance RightBoundedSequence Bool where
    lastElem = True

