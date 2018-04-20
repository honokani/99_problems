module Lib
    ( solveP15
    ) where

solveP15 :: IO ()
solveP15 = do
    print $ repl "abc" 3

repl :: [a] -> Int -> [a]
repl w i = concatMap f w
    where f = (take i).repeat

