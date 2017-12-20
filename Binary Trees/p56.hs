-- Check if a binary tree is symmetric

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

leaf x = Branch x Empty Empty

checkSym :: Tree a -> Boolean
--[WIP]