-- Rotate a list 

rotateList:: (Ord a) => [a] -> Int -> [a]
rotateList xs n
    | length xs <= 1 = xs
    | n < 0 = rotateList xs ((length xs) - abs n)
    | length xs <= n = rotateList xs (mod (length xs) n)
    | otherwise = drop rest xs ++ take rest xs
        where 
            rest = length xs - n