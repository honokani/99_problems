module Lib
    ( solveP63
    ) where

import Data.List

solveP63 :: IO ()
solveP63 = do
    print $ completeBinalyTree 6 'x'
    print $ isCompleteBinalyTree $ completeBinalyTree 6 'x'
    print $ isCompleteBinalyTree (B 1 (B 2 E (B 2 E E)) (B 3 (B 3 (B 3 E E) E) E))
    print $ isCompleteBinalyTree $ completeBinalyTree (2^8 - 100) 'x'


data Tree a = E
            | B a (Tree a) (Tree a)
            deriving (Show, Eq)

completeBinalyTree :: (Integral a) => a -> b -> Tree b
completeBinalyTree 0 _ = E
completeBinalyTree n o = makeTree 1
    where
        makeTree i = B o (addCheck makeTree $ 2*i) (addCheck makeTree $ 2*i+1)
        addCheck f x | (x <= n)  = f x
                     | otherwise = E

countHight :: (Integral a) => Tree b -> a
countHight E = 0
countHight (B _ l r) = big + 1
    where
        lh = countHight l
        rh = countHight r
        big = if lh < rh then rh else lh

isCompleteBinalyTree :: Tree b -> Bool
isCompleteBinalyTree t = (<= 2).length.squash.concat.take (countHight t) $ checkExistance t
    where
        squash [x] = [x]
        squash (x:y:z) = if x == y then squash (y:z) else x:squash (y:z)

checkExistance :: Tree a -> [[Bool]]
checkExistance E = cycle [[False]]
checkExistance (B _ l r) = [[True]] ++ child
    where
        tsl = checkExistance l
        tsr = checkExistance r
        child = zipWith (++) tsl tsr

