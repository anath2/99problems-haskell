-- Find the last but one element of the list

findSecondLast :: [a] -> a
-- TODO throw an exception if list has just one element
findSecondLast [x] = x
findSecondLast [x, y] = x
findSecondLast (x:xs) = findSecondLast xs

