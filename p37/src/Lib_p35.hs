module Lib_p35
    ( primeFactors
    ) where

import Lib_p31 as P31

solveP35 :: IO ()
solveP35 = print $ primeFactors 315

primeFactors :: (Integral a) => a -> [a]
primeFactors n = primeFactorsCore n $ P31.pCandidates n

primeFactorsCore :: (Integral a) => a -> [a] -> [a]
primeFactorsCore 0 _  = []
primeFactorsCore _ [] = []
primeFactorsCore i allp@(p:ps) = case mod i p of
    0 -> p:primeFactorsCore (div i p) allp
    _ -> primeFactorsCore i ps

