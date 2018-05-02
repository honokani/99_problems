module Lib
    ( solveP31
    ) where

solveP31 :: IO ()
solveP31 = do
    print $ isPrime 9029399
    print $ isPrime 9029400

isPrime :: (Integral a) => a -> Bool
isPrime n = (==[]) $ filter ((==0).(mod n)) $ take (getSquareN n) primes

getSquareN :: (Integral a) => a -> Int
getSquareN = floor.sqrt.realToFrac

primes :: (Integral a) => [a]
primes = [ x | x <- primeCore [2..] ]
    where primeCore (n:ns) = n : primeCore [ x | x <- ns, mod x n /= 0 ]

