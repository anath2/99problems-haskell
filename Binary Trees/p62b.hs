-- Collect all nodes of a tree at nth level

data Tree a = Empty | Branch a (Tree a) (Tree a)
    deriving (Eq, Show)

atLevel :: (Tree a) -> Int -> [a]
