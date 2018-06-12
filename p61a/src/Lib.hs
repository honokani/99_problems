module Lib
    ( solveP61a
    ) where

solveP61a :: IO ()
solveP61a = do
    print $ leaves (B 'a' (B 'b' E E) (B 'c' E (B 'd' E E)))


data Tree a = E
            | B a (Tree a) (Tree a)
            deriving (Show, Eq)

leaves :: Tree a -> [a]
leaves E = []
leaves (B x E E) = [x]
leaves (B x l r) = leaves l ++ leaves r

