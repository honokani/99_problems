module Lib
    ( solveP37
    ) where

import Lib_p35 as P35
import Control.Arrow

solveP37 :: IO ()
solveP37 = do
    print $ entupleFactors $ P35.primeFactors 315
    print $ totientPhi 315

totientPhi :: (Integral a) => a -> a
totientPhi m = foldr (*) 1 [ (p-1) * p^(i-1) | (p,i) <- entupleFactors.primeFactors $ m ]

entupleFactors :: (Integral a) => [a] ->[(a,Int)] 
entupleFactors [x] =[(x, 1)]
entupleFactors (x:xs) = case fst tHead == x of
    True -> (id *** (+1)) tHead : tail rest
    _    -> (x,1) : rest
    where rest = entupleFactors xs
          tHead = head rest

