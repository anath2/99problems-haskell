-- Check if a binary tree is symmetric

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

leaf x = Branch x Empty Empty

checkSym Empty Empty = True
checkSym (Branch _ a b) (Branch _ x y) = checkSym a y && checkSym b x
-- Else
mirror _ _ = False
 
symmetric Empty = True
symmetric (Branch _ l r) = checkSym l r