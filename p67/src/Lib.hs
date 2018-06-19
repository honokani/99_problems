module Lib
    ( solveP67
    ) where

import           Control.Applicative      ((<|>))
import qualified Text.Parsec        as P
import qualified Text.Parsec.String as PS

solveP67 :: IO ()
solveP67 = do
    runParse pTree "a(,)"
    runParse pTree ""
    runParse pTree "a(b(,),)"
    runParse pTree "a(b(d,e),c(,f(g,)))"

runParse :: (Show a) => PS.Parser a -> String -> IO ()
runParse p i = case P.parse p "p67_StrToTree" i of
    Left err -> print $ "error : " ++ show err
    Right x  -> print x

data Tree a = E
            | B a (Tree a) (Tree a)
            deriving (Show, Eq)

type CTree = Tree Char

pTree :: PS.Parser CTree
pTree = do  x <- P.letter
            do  P.char '('
                l <- pTree
                P.char ','
                r <- pTree
                P.char ')'
                return $ B x l r
                <|>
                do return $ B x E E
            <|>
            return E

