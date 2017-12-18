-- Construct completely balanced binary trees
--[WIP]
-- Construct permutations of balanced binary trees from the number of nodes

data Tree a = Empty | Branch a (Tree a) (Tree a)
    deriving (Show, Eq)

cbTrees :: Int -> [Tree Char]
cbTrees 0 = [Empty]

cbalTree n = let (q, r) = (n - 1) `quotRem` 2
    in [
        Branch 'x' left right | i <- [q .. q + r], left  <- cbalTree i,
        right <- cbalTree (n - i - 1)
    ]