data Tree a = Node a [Tree a] 
    deriving (Eq, Show)

countNodes :: Tree Char -> Int  

countNodes Node _ ts = 1 + sum (map nnodes ts)
