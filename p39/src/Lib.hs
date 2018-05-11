module Lib
    ( solveP39
    ) where

import Lib_p31 as P31

solveP39 :: IO ()
solveP39 = print $ listPrimesInRange 7 32

listPrimesInRange :: (Integral a) => a -> a -> [a]
listPrimesInRange l u = takeWhile (<=u) $ dropWhile (<l) P31.primes

