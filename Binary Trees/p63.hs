-- Construct a complete binary tree

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Eq, Show)

completeBinaryTree :: a -> Int -> (Tree a)
