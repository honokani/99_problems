module Lib
    ( solveP59
    ) where

import Data.List as DL

solveP59 = do
    print $ hbalTrees 0 3

data Tree a = E
            | B a (Tree a) (Tree a)
            deriving (Show, Eq)

hbalTrees :: (Integral a, Eq b) => b -> a -> [Tree b]
hbalTrees x 0 = [E]
hbalTrees x 1 = [B x E E]
hbalTrees x n = [ B x l r | (il,ir) <- [(n-1,n-1), (n-1,n-2), (n-2,n-1)]
                          , l <- hbalTrees x il
                          , r <- hbalTrees x ir
                ]

