--Insert all the internal nodes of a tree into a list
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Eq, Show)
internalNodes :: (Tree a) -> [a]

internalNodes Empty = []
internalNodes (Branch x Empty Empty) = []
internalNodes (Branch x l r) = x : internalNodes l ++ internalNodes r
