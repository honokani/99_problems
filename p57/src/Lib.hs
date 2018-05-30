module Lib
    ( solveP57
    ) where

solveP57 :: IO ()
solveP57 = do
    print $ treeFromList [3,2,5,7,1]


data Tree a = E
            | B a (Tree a) (Tree a)
            deriving (Show, Eq)

treeFromList :: (Integral a) => [a] -> Tree a
treeFromList xs = foldl (\acc x -> addVal x acc) E xs

addVal :: (Integral a) => a -> Tree a -> Tree a
addVal x E = B x E E
addVal x tgt@(B y l r)
    | x == y     = tgt
    | x < y      = B y (addVal x l) r
    | otherwise  = B y l (addVal x r)

