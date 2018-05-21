module Lib
    ( solveP55
    ) where

import Data.List as DL

solveP55 :: IO ()
solveP55 = do
    print $ allTree  3
    print $ cbalTree 3

data Tree a = E
            | B a (Tree a) (Tree a)
            deriving (Show, Eq)
type CTree = Tree Char

leaf :: CTree
leaf = B 'x' E E
node :: CTree -> CTree -> CTree
node l r = B 'x' l r

allTree :: (Integral a) => a -> [CTree]
allTree 0 = [E]
allTree n = addLeaf $ allTree (n-1)

addLeaf :: [CTree] -> [CTree]
addLeaf = DL.nub.concat.map addLeafCore

addLeafCore :: CTree -> [CTree]
addLeafCore t = case t of
    E -> [leaf]
    _ -> grow $ splitTree t

splitTree :: CTree -> (CTree, CTree)
splitTree (B _ l r) = (l,r)

grow :: (CTree, CTree) -> [CTree]
grow (l,r) = [node gl r | gl <- addLeafCore l] ++ [node l gr | gr <- addLeafCore r]

cbalTree :: (Integral a) => a -> [CTree]
cbalTree 0 = allTree 0
cbalTree n = addCbalLeaf $ cbalTree (n-1)

addCbalLeaf :: [CTree] -> [CTree]
addCbalLeaf t = filter isBalanced $ addLeaf t

isBalanced :: (Tree a) -> Bool
isBalnaced E = True
isBalanced (B _ l r) =  hl - hr < 2  && hr - hl < 2
                     && isBalanced l && isBalanced r
    where   hl = countHight l
            hr = countHight r
isBalanced _ = True

countHight :: (Integral b) => Tree a -> b
countHight E = 0
countHight (B _ l r) = 1 + countHight l + countHight r

