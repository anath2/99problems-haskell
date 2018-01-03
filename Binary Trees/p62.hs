-- Collect leaves of a binary tree in a list
data Tree a = Empty | Branch a (Tree a) (Tree a)
collectLeaves :: (Tree a) -> [a]

collectLeaves Empty = []
collectLeaves Branch x Empty Empty = [x]
collectLeaves Branch _ (Tree l) (Tree r) = collectLeaves l ++ collectLeaves r

