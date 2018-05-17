module Lib
    ( solveP55
    ) where

import Data.List as DL

solveP55 :: IO ()
solveP55 = do
    print $ length $ cbalTree 11

data Tree a = E
            | B a (Tree a) (Tree a)
            deriving (Show, Eq)
type CTree = Tree Char

leaf :: CTree
leaf = B 'x' E E
node :: CTree -> CTree -> CTree
node l r = B 'x' l r

cbalTree :: (Integral a) => a -> [CTree]
cbalTree 0 = [E]
cbalTree n = addLeaf $ cbalTree (n-1)

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

