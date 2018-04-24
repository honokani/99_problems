module Lib
    ( solveP27
    ) where

solveP27 :: IO ()
solveP27 = do
    print $ combination 1 "abc"

combination :: Int -> [a] -> [([a],[a])]
combination 0 xs = [([],xs)]
combination _ [] = []
combination n (x:xs) = case n < 0 of
    True -> error "Out of bounds!"
    _    -> picked ++ rests
    where picked = [(x:pa, pb) | (pa,pb) <- combination (n-1) xs]
          rests  = [(ra, x:rb) | (ra,rb) <- combination n xs]

