-- Calculate internal path length

data Tree a = Node a [Tree a]

pathLength :: Tree a -> Int
pathLength = pathLength' 0
    where
        pathLength' d (Node _ ts) = d + sum (map (pathLength' (d + 1)) ts)
