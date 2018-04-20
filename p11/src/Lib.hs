module Lib
    ( solveP11
    ) where

data Counter a = S a
               | M Int a
               deriving (Show)

solveP11 :: IO ()
solveP11 = do
    print $ encodeModified "aaaabccaadeeee"

encodeModified :: (Eq a) => [a] -> [Counter a]
encodeModified = foldr f []

f :: (Eq a) => a -> [Counter a] -> [Counter a]
f x [] = [S x]
f x acc@((S y):z) = case x==y of
    True -> (M 2 y):z
    _    -> (S x):acc
f x acc@((M n y):z) = case x==y of
    True -> (M (n+1) y):z
    _    -> (S x):acc

