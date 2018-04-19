module Lib
    ( solveP01
    ) where


solveP01 :: IO()
solveP01 = print $ p01Last [1, 1, 2, 3, 5, 8]

p01Last :: [a] -> a
p01Last [] = error "empty list"
p01Last [x] = x
p01Last (_:xs) = p01Last xs

