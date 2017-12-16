-- Construct completely balanced binary trees
--[WIP]
-- Construct permutations of balanced binary trees from the number of nodes

data Tree a = Empty | Branch a (Tree a) (Tree a)
    deriving (Show, Eq)

cbTrees :: Int -> [Tree Char]