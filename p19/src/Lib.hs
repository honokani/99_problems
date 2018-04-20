module Lib
    ( solveP19
    ) where

solveP19 :: IO ()
solveP19 = do
    print $ shift "abcdefgh" 5
    print $ shift "abcdefgh" (-2)

shift :: [a] -> Int -> [a]
shift xs 0 = xs
shift xs n = case n<0 of
    True -> shift xs $ n+l
    _    -> take l . drop n $ cycle xs
    where l = length xs

