module Lib
    ( solveP21 
    ) where

solveP21 :: IO ()
solveP21 = do
    print $ insertAt "alfa" ["a","b","c","d"] 2


insertAt :: a -> [a] -> Int -> [a]
insertAt x [] 1 = [x]
insertAt _ [] _ = error "Out of bounds!"
insertAt x ally@(y:ys) n
    | n==1       = x : ally
    | otherwise  = y : insertAt x ys (n-1)

