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