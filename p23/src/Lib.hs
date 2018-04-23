module Lib
    ( solveP23
    ) where

import System.Random as Rnd

solveP23 :: IO ()
solveP23 = do
    print =<< randomSelect "abcdefghijkl" 4
    a <- ( Rnd.getStdRandom $ Rnd.randomR (0,10) :: IO Int)
    print $ (:[]) a
--    a <- randomSelect "someFunc" 3
--    print a




randomSelect :: [a] -> Int -> IO [Int]
randomSelect xs n
    | n < 0     = error "select number is smaller than 0!"
    | n == 0    = return [99]
    | otherwise = do
        r <- Rnd.getStdRandom $ Rnd.randomR (0,10)
        randomSelect xs (n-1)
        return [r]

