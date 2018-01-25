-- Check whether a given term represents a multiway tree

-- Trees can be defined as the following by the following 
-- recursive structure

data Tree a = Node a [Tree a]  
    deriving (Eq, Show)

