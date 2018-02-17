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

iso :: (Ord a, Enum a, Ord b, Enum b) => Graph a -> Graph b -> Bool
iso g@(Graph xs ys) h@(Graph xs' ys') = length xs == length xs' &&
                                        length ys == length ys' &&
                                        canon g == canon h

canon :: (Ord a, Enum a) => Graph a -> String