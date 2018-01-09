-- Collect all nodes of a tree at nth level

data Tree a = Empty | Branch a (Tree a) (Tree a)
    deriving (Eq, Show)

atLevel :: (Tree a) -> Int -> [a]
atLevel Empty _ = []
atLevel (Branch x l r) n 
    | n < 1         = []
    | n == 1        = [x]
    | otherwise     = atLevel l (n - 1) ++ atLevel r (n - 1)
