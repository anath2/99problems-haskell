-- Generate K regular simple graphs 

data Graph a = Graph [a] [(a, a)]
                        deriving (Show, Eq)
data Adjacency a = Adj [(a, [a])]
                        deriving (Show, Eq)

regular :: Int -> Int -> [Graph Int]
regular n k 
    | r == 1 || n <= k || n < 0 || k < 0 = []
    | otherwise = map (adjToGraph . fst) $ 
        foldr (\x xs -> if any ((==) (snd x) . snd) xs then xs else x : xs) [] $ 
        zip a $ map canon a
    
    where
        a = filter (\(Adj a) -> all ((==) k . length . snd) a) $
            map (graphToAdj . Graph [1..n]) $ perm e q
        e = map (\xs -> (head xs, last xs)) $ perm [1..n] 2
        (q, r) = (n * k) `quotRem` 2
        perm n k = foldr (\x xs -> 
                        [i : s | i <- n, s <- xs, i `notElem` s, asc (i : s)])
                        [[]] [1..k]
        asc xs = all (uncurry (<)) $ zip xs $ tail xs