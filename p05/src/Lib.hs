module Lib
    ( solveP05
    ) where

solveP05 :: IO ()
solveP05 = print $ myReverse "My reverese function"

myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]

