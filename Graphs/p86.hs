-- Graph coloration using Welch Powell's algorithm

data Graph a        = Graph [a] [(a, a)]
                        deriving (Eq, Show)

data Adjacency a    = Adj [(a, [a])]
                        deriving (Eq, Show)
                        
kcolor :: (Eq a, Ord a) => Graph a -> [(a, Int)]
kcolor g = kcolor' x [] 1
    where
        Adj x = sortg g
        kcolor' [] ys _ = ys
        kcolor' xs ys n = let ys' = color xs ys n
                        in kcolor' [x | x <- xs, notElem (fst x, n) ys']
                    ys'
                    (n + 1)
        color []          ys n = ys
        color ((v, e):xs) ys n = if any (\x -> (x, n) `elem` ys) e
                                then color xs ys n
                                else color xs ((v, n) : ys) n

-- determines chromatic number, given graph coloration
chromatic :: [(a, Int)] -> Int
chromatic x = length $ foldr (\(a, n) xs -> if n `elem` xs then xs else n : xs) [] x

-- Graph to ajdacency representation
graphToAdj :: (Eq a) => Graph a -> Adjacency a
graphToAdj (Graph [] _)      = Adj []
graphToAdj (Graph (x:xs) ys) = Adj ((x, ys >>= f) : zs)
     where 
        f (a, b) 
            | a == x = [b]
            | b == x = [a]
            | otherwise = []
        Adj zs = graphToAdj (Graph xs ys)

-- Graph sorted by node degree
sortg :: (Eq a, Ord a) => Graph a -> Adjacency a
sortg g = Adj $ map (\(a, b) -> (a, sort b 1 maximum)) $ sort x 1 maxv
    where
        Adj x = graphToAdj g
        sort [] _ _ = []
        sort xs n f = let m = f xs in
            m : sort [x | x <- xs, x /= m] (n + 1) f
        maxv (x:xs) = foldr (\a@(a1, _) b@(b1, _) -> if a1 > b1 then a else b) x xs
