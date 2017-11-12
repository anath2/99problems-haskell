-- Remove the nth element from the lis

removeNth :: [a] -> Int -> (a, [a])
removeNth xs n
    | n < 0 = removeNth xs ((length xs) - (abs n))
    | otherwise = (xs!!n, take n xs ++ drop (n + 1) xs)
