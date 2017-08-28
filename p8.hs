-- Flatten a list removing consecutive element that are same

flattenList :: (Eq a) => [a] -> [a]

flattenList [] = []
flattenList [a] = [a]

flattenList (x:xs)  
    | ( x == head xs) = flattenList xs
    | otherwise = [x] ++ flattenList xs
