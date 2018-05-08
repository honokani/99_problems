module Lib
    ( solveP33
    ) where

solveP33 :: IO ()
solveP33 = do
    print $ isCoprime 33 65
    print $ isCoprime 33 66

isCoprime :: (Integral a) => a -> a -> Bool
isCoprime i j = (== 1) $ gcb i j

gcb :: (Integral a) => a -> a -> a
gcb i j
    | i < j     = gcb j i
    | otherwise = let k = mod i j in
        case k of
            0 -> j
            1 -> 1
            _ -> gcb j k

