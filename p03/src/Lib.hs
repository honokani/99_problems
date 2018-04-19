module Lib
    ( solveP03
    ) where

solveP03 :: IO ()
solveP03 = print $ kthElememt [10, 20, 30, 40, 50] 2

kthElememt :: [a] -> Int -> a
kthElememt [] _ = error "list too short"
kthElememt (x:_) 1 = x
kthElememt (_:xs) n = case n<1 of
    True  -> error $ "there is no " ++ show n ++ "th position"
    False -> kthElememt xs $ n-1

