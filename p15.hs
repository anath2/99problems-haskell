-- Replicate the elements of the list n times
repli :: (Eq a) => [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n 
    | n == 1 = (x:xs)
    | otherwise = (helper x n) ++ repli xs n
        where
            helper x 1 = [x]
            helper x n = [x] ++ (helper x (n - 1))