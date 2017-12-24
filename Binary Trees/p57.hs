-- Construct a binary tree from a list of integers

-- Define a tree datatype
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

-- Create an add function to add to a tree
add :: Ord a => a -> Tree a -> Tree a
add x Empty            = Branch x Empty Empty
add x t@(Branch y l r) = case compare x y of
                            LT -> Branch y (add x l) r
                            GT -> Branch y l (add x r)
                            EQ -> t

-- Use fold to add to an empty tree 
constructTree xs = foldl (flip add) Empty xs 