module Lib
    ( solveP49
    ) where

solveP49 :: IO ()
solveP49 = do
    print $ gray 3

gray 0 = [""]
gray n = map ('0':) prev ++ map ('1':) (reverse prev)
    where !prev = gray $ n-1

