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
canon g = minimum $ map f $ perm $ length a
    where
        Adj a = graphToAdj g
        v = map fst a
        perm n = foldr (\x xs -> [i : s | i <- [1..n], s <- xs, i `notElem` s]) [[]] [1..n]
        f p = let n = zip v p
            in show [(snd x,
                sort id $ map (\x ->
                    snd $ head $ snd $ break ((==) x . fst) n) $ snd $ find a x)
                | x <- sort snd n]
        sort f n = foldr (\x xs -> let (lt, gt) = break ((<) (f x) . f) xs
                                   in lt ++ [x] ++ gt) [] n
        find a x = let (xs, ys) = break ((==) (fst x) . fst) a in head ys