module Lib
    ( solveP65
    ) where

solveP65 :: IO ()
solveP65 = do
    print $ (layoutBinalyTree65 (B "1-1" E (B "2-1" E E)) :: Tree ([Char],(Int,Int)))
--    print $ layoutBinalyTree65 (B "1-2" (B "2-1" E E) E)
--    print $ layoutBinalyTree65 (B "1-2" (B "2-1" E E) (B "2-3" E E))
--    print $ layoutBinalyTree65 (B "1-3" (B "2-1" E (B "3-2" E E)) (B "2-4" E E))
--    print $ layoutBinalyTree65 (B "1-3" (B "2-1" E (B "3-2" E E)) (B "2-4" E E))
    print $ ( layoutBinalyTree65 (
                B 'n' (B 'k' (B 'c' (B 'a' E E) (B 'e' (B 'd' E E) (B 'g' E E))) (B 'm' E E))
                      (B 'u' (B 'p' E (B 'q' E E)) E)
              ) :: Tree (Char,(Int,Int)))


data Tree a = E
            | B a (Tree a) (Tree a)
            deriving (Show, Eq)

layoutBinalyTree65 :: (Integral a, Bounded a) => Tree b -> Tree (b,(a,a))
layoutBinalyTree65 E = E
layoutBinalyTree65 t = normalizePosition.fst $ decideLayout t 1 0
    where
        mDepth = maxDepth t
        decideLayout E _ _ = (E,0)
        decideLayout (B x l r) currDepth here = (B (x,(currDepth,arm)) placedL placedR, here)
            where
                arm = if currDepth == mDepth then 0 else (2^(mDepth-currDepth-1)-1)
                (placedL,wl) = decideLayout l (currDepth+1) (here-arm-1)
                (placedR,wr) = decideLayout r (currDepth+1) (here+arm+1)

normalizePosition :: (Integral a, Bounded a) => Tree (b,(a,a)) -> Tree (b,(a,a))
normalizePosition t = positionShift t
    where
        getMinimumPos E = maxBound
        getMinimumPos (B (_,(_,w)) l r) = foldr min maxBound [w,getMinimumPos l,getMinimumPos r]
        n = getMinimumPos t
        positionShift E = E
        positionShift (B (x,(d,w)) l r) = (B (x,(d,(w-n))) (positionShift l) (positionShift r))



widthGrandBranchs E = (0,0)
widthGrandBranchs (B x E E) = (0,0)
widthGrandBranchs (B x l r) = (nl,nr)
    where
        (nll,nlr) = widthGrandBranchs l
        (nrl,nrr) = widthGrandBranchs r
        nl = (+1).(*2) $ max nll nlr
        nr = (+1).(*2) $ max nrl nrr

maxDepth :: (Integral a) => Tree b -> a
maxDepth E = 0
maxDepth (B _ l r) = max lh rh + 1
    where
        lh = maxDepth l
        rh = maxDepth r

