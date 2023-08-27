module Template where

import Control.Monad

prog1 :: IO ()
prog1 = do  m <- readLn :: IO Int
            n <- readLn :: IO Int
            -- replicateM_ m (print n)
            repeat m (print n)

prog1b :: IO ()
prog1b = error "not implemented"

prog2 :: IO ()
prog2 = error "not"

index :: [IO a] -> IO Int -> IO a
index = error "not"
