-- Construct a bottom up order of tree notes
data Tree a = Node a [Tree a]

bottomUpTree :: Tree a -> String
