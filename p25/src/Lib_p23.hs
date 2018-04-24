module Lib_p23
    ( solveP23
    , randomSelect
    ) where

import System.Random as Rnd

solveP23 :: IO ()
solveP23 = do
    print =<< randomSelect "abcdefghijkl" 4
--  a <- ( Rnd.getStdRandom $ Rnd.randomR (0,10) :: IO Int)
--  print $ (:[]) a
--  a <- randomSelect "someFunc" 3
--  print a



randomSelect :: [a] -> Int -> IO [a]
randomSelect xs n
    | n < 0     = error "select number is smaller than 0!"
    | n == 0    = return []
    | otherwise = do
        r <- Rnd.getStdRandom $ Rnd.randomR (0, length xs - 1)
        (xs!!r :) <$> randomSelect (removeAt xs r) (n-1)

removeAt :: [a] -> Int -> [a]
removeAt [] _ = error "Out of bounds!"
removeAt (x:xs) 0 = xs
removeAt (x:xs) n = case n < 0 of
    True -> error "Out of bounds!"
    _    -> x : removeAt xs (n-1)

