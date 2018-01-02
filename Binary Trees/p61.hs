-- Count the number of leaves in a binary search tree

data Tree a = Empty | Branch a (Tree a) (Tree a)

countLeaves :: (Tree a) -> Int

countLeaves Empty                   = 0
countLeaves (Branch _ Empty Empty)  = 1
countLeaves (Branch _ l r)          = countLeaves l + countLeaves r
