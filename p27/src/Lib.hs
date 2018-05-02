module Lib
    ( solveP27
    ) where

solveP27 :: IO ()
solveP27 = do
    print $ group [1,1] "abc"
    print $ length $ group [2,2,5] ["a","b","c","d","e","f","g","h","i"]

combination :: Int -> [a] -> [([a],[a])]
combination 0 xs = [([],xs)]
combination _ [] = []
combination n (x:xs) = case n < 0 of
    True -> error "Out of bounds!"
    _    -> picked ++ rests
    where picked = [(x:pa, pb) | (pa,pb) <- combination (n-1) xs]
          rests  = [(ra, x:rb) | (ra,rb) <- combination n xs]

group :: [Int] -> [a] ->[[[a]]] 
group _ [] = [[]]
group [] _ = [[]]
group (x:xs) cs = [ f:semiGroup | (f,s) <- sliced, semiGroup <- group xs s ]
    where sliced = combination x cs

