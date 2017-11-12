-- Find the kth element of a list
findKthElement :: [a] -> Int -> a
findKthElement (x:xs) 0 = x
findKthElement (x:xs) k = findKthElement xs (k - 1)