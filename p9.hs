-- ADD THE REPEATING ELEMENTS OF A LIST INTO A SUBLIST

packList :: (Eq a) => [a] -> [[a]]
packList [] = []
packList [x] = [[x]]
packList (x:xs) = if x `elem` (head (packList xs))
                  then (x: (head (packList xs)))  : (tail $ packList xs)
                  else [x]: (packList xs)