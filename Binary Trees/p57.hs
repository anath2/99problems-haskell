-- Construct a binary tree from a list of integers

-- Define a tree datatype
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
-- leaf x = Branch x Empty Empty

-- Define a function to add an element to a tree
-- add :: Ord a => a -> Tree a -> Tree a
-- add x Empty = leaf xa
-- add x t@(Branch y l r) = case compare x y of
--                             LT -> Branch y (add x l) r
--                             GT -> Branch y l (add x r)
--                             EQ -> t

-- constructTree xs = foldl (flip add) Empty xs

add :: Ord a => a -> Tree a -> Tree a
add x Empty            = Branch x Empty Empty
add x t@(Branch y l r) = case compare x y of
                            LT -> Branch y (add x l) r
                            GT -> Branch y l (add x r)
                            EQ -> t

constructTree xs = foldl (flip add) Empty xs 