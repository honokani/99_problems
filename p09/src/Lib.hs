module Lib
    ( solveP09
    ) where

solveP09:: IO ()
solveP09 = do
    print $ packList "aaaabccaadeeee"

packList :: (Eq a) => [a] -> [[a]]
packList = foldr f []

f :: (Eq a) => a -> [[a]] -> [[a]]
f x [] = [[x]]
f x ((y:ys):z) = case x==y of
    True -> (y:y:ys):z
    _    -> [x]:(y:ys):z
