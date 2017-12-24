-- Construct all symmetric and completely balanced trees 

-- Construct completely balanced trees from a collection of trees

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Eq, Show)

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n = let (q, r) = (n - 1) `quotRem` 2
    in [Branch 'x' left right | i     <- [q .. q + r],
                                left  <- cbalTree i,
                                right <- cbalTree (n - i - 1)]

symCbalTrees n 
    | n `mod` 2 == 0 = []
    | otherwise = [Branch 'x' t (reverseTree t) | t <- cbalTree n]
        where 
            reverseTree Empty = Empty
            reverseTree (Branch x l r) = Branch x (reverseTree r) (reverseTree l)