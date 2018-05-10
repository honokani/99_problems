module Lib_p31
    ( pCandidates
    ) where

solveP31 :: IO ()
solveP31 = do
    print $ isPrime 9029399
    print $ isPrime 9029400

isPrime :: (Integral a) => a -> Bool
isPrime n = (==[]) $ filter ((==0).mod n) $ pCandidates n

pCandidates :: (Integral a) => a -> [a]
pCandidates n = takeWhile (<= getSquareN n) primes

getSquareN :: (Integral a) => a -> a
getSquareN = floor.sqrt.realToFrac

primes :: (Integral a) => [a]
primes = primeCore [2..]
    where primeCore (n:ns) = n : primeCore [ x | x <- ns, mod x n /= 0 ]

