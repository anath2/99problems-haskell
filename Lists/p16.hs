-- Drop every nth element from the list

dropNth :: Int -> [a] -> [a]
dropNth _ [] = []
dropNth n xs = take (n-1) xs ++ dropNth n (drop n xs) 