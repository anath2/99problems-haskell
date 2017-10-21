-- Rotate a list 

rotateList:: (Ord a) => [a] -> Int -> [a]
-- List with zero elements
rotateList xs n
    | length xs <= 1 = xs
    | length xs <= n = rotateList xs (mod (lenth xs) n)
    | otherwise = drop rest xs ++ take rest xs
        where 
            rest = length xs - n