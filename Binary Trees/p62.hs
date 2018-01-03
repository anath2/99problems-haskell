-- Collect leaves of a binary tree in a list
data Tree a =  Empty | Branch a (Tree a) (Tree a) 
    deriving (Eq, Show)
leaves :: Tree a -> [a]
leaves  Empty                 = []
leaves (Branch a Empty Empty) = [a]
leaves (Branch a left right) = leaves left ++ leaves right

