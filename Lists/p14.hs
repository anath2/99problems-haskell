-- Duplicate elements of a list

duplicateElements :: (Eq a) => [a] -> [a]
duplicateElements [] = []
duplicateElements (x:xs) = [x, x] ++ (duplicateElements xs)
