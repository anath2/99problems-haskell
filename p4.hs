-- Find the number of elements in the list

length' :: [a] -> Int
length' [x] = 1
length' (x:xs) = 1 + length' xs