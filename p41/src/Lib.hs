module Lib
    ( solve41
    ) where

import Lib_p31 as P31

solve41 :: IO ()
solve41 = do
    print $ goldbach 28

goldbach :: (Integral a) => a -> (a,a)
goldbach n = head [ (x,y) | x <- candiP, y <- candiP, n==x+y]
    where candiP = takeWhile (<=n) P31.primes

