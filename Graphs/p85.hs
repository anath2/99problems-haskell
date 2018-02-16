-- Check if the graphs are isomorphic

data Graph a = Graph [a] [(a, a)]
                    deriving (Eq, Show)
data Adjacency a = Adj [(a, [a])]
                    deriving (Eq, Show)

graphToAdj :: (Eq a) => Graph a -> Adjacency a
graphToAdj (Graph [] _)      = Adj []
graphToAdj (Graph (x:xs) ys) = Adj ((x, ys >>= f) : zs)
    where
        f (a, b)
            | a == x = [b]
            | b == x = [a]
            | otherwise = []
        Adj zs = graphToAdj (Graph xs ys)