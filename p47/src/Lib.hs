module Lib
    ( solveP47
    ) where

import Control.Monad

solveP47 :: IO ()
solveP47 = do
    table (\a b -> a `and'` (a `or'` not' b))

table :: (Bool -> Bool -> Bool) -> IO ()
table f = do
    forM_ [ show a ++ " " ++ show b ++ " " ++ show (f a b) | a <- tf, b <- tf] $
        print
    where tf = [True, False]

and' :: Bool -> Bool -> Bool
and' True  x = x
and' False _ = False

or' :: Bool -> Bool -> Bool
or' False x = x
or' True  _ = True

not' :: Bool -> Bool
not' True = False
not' _    = True

infixl 4 `or'`
infixl 6 `and'`
infixl 9 `not'`
