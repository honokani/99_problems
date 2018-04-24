module Lib
    ( solveP25
    ) where

import Lib_p23 as L23

solveP25 :: IO ()
solveP25 = print =<< permutation "abcdefghi"

permutation :: [a] -> IO [a]
permutation xs = L23.randomSelect xs (length xs)

