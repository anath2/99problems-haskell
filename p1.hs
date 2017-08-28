-- Find the last element of the array

findLast :: [a] -> a
findLast [x] =  x 
findLast (x : xs) = findLast xs