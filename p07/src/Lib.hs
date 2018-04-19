module Lib
    ( solveP07
    ) where

data Tree a = E
            | L a
            | B [Tree a]
            deriving (Eq, Ord, Show)

solveP07 :: IO ()
solveP07 = do
    print $ flatten $ B [ B [L 1, L 1], L 2, B [L 3, B [L 5, L 8]] ]

flatten :: Tree a -> [a]
flatten E         = []
flatten ( B [] )  = []
flatten ( B [E] ) = []
flatten ( L x )     = [x]
flatten ( B [L x] ) = [x]
flatten ( B (x:xs) ) = case x of
    E   ->              flatten (B xs)
    b   -> flatten b ++ flatten (B xs)

