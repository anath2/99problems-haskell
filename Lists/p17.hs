-- Split the list into two parts
splitList :: [a] -> Int -> [[a]]
splitList xs n = takeN n xs : (dropN n xs : [])
    where
        takeN 0 (x:xs) = []
        takeN n (x:xs)
            | n >= (length (x:xs)) = (x:xs)
            | otherwise = [x] ++ takeN (n-1) xs
        
        dropN 0 (x:xs) = (x:xs)
        dropN n (x:xs) 
            | n >= (length (x:xs)) = [] 
            | otherwise = [] ++ dropN (n-1) xs
