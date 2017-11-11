-- Generate combinations of element in sublists from a list
-- The sublist sizes of sublists being given as list argument

combination :: Int -> [a] -> [([a], [a])] 
combination 0 xs        = [([], xs)]
combination n []        = []
combination n (x:xs)    = ts ++ ds
    where
        ts = [ (x:ys, zs) | (ys, zs) <- combination (n - 1) xs ]
        ds = [ (ys, x:zs) | (ys, zs) <- combination n       xs ]

groupElem :: [Int] -> [a] -> [[[a]]]
groupElem [] xs = [[]]
groupElem (g:gs) xs = concatMap helper $ combination g xs
            where helper (as, bs) = map (as:) (groupElem gs bs)

    