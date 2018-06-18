module Lib_another
    ( solveP65
    ) where

solveP65 :: IO ()
solveP65 = do
    print $ layoutBinalyTree65 (B "1-1" E E)
    print $ layoutBinalyTree65 (B "1-1" E (B "2-1" E E))
    print $ layoutBinalyTree65 (B "1-2" (B "2-1" E E) E)
    print $ layoutBinalyTree65 (B "1-2" (B "2-1" E E) (B "2-3" E E))
    print $ layoutBinalyTree65 (B "1-3" (B "2-1" E (B "3-2" E E)) (B "2-4" E E))
    print $ layoutBinalyTree65 (B "1-3" (B "2-1" E (B "3-2" E E)) (B "2-4" E E))
    print $ layoutBinalyTree65 (
                B 'n' (B 'k' (B 'c' (B 'a' E E) (B 'e' (B 'd' E E) (B 'g' E E))) (B 'm' E E))
                      (B 'u' (B 'p' E (B 'q' E E)) E)
                               )


data Tree a = E
            | B a (Tree a) (Tree a)
            deriving (Show, Eq)

layoutBinalyTree65 :: (Integral a) => Tree b -> Tree (b,(a,a))
layoutBinalyTree65 E = E
layoutBinalyTree65 t = fst $ decideLayout t 1 0

decideLayout E _ _ = (E,0)
decideLayout (B x l r) depth shift = (B (x,(depth,here)) placedL placedR, here+wr-shift)
    where
        (placedL,wl) = decideLayout l (depth+1) (shift)
        (nll,nlr) = widthGrandBranchs l
        (nrl,nrr) = widthGrandBranchs r
        arm = foldr max 0 [nll,nlr,nrl,nrr]
        here = wl-nlr+arm+1+shift
        (placedR,wr) = decideLayout r (depth+1) (here+arm)

widthGrandBranchs E = (0,0)
widthGrandBranchs (B x E E) = (0,0)
widthGrandBranchs (B x l r) = (nl,nr)
    where
        (nll,nlr) = widthGrandBranchs l
        (nrl,nrr) = widthGrandBranchs r
        nl = (+1).(*2) $ max nll nlr
        nr = (+1).(*2) $ max nrl nrr

