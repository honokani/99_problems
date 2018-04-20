module Lib
    ( solveP17
    ) where

solveP17 :: IO ()
solveP17 = do
    print $ mySplit "abcdefghijkl" 3

mySplit :: [a] -> Int -> ([a], [a])
mySplit [] _ = ([], [])
mySplit allx@(x:xs) n = case n<1 of
    True -> ([], allx)
    _    -> let (ys, zs) = mySplit xs (n-1) in (x:ys, zs)

