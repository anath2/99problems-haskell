-- Run length encoding algorithm 
-- Consecutive duplicates of elements are encoded as lists (N E) 
-- where N is the number of duplicates of the element E.

packList :: (Eq a) => [a] -> [[a]]
packList [] = []
packList [x] = [[x]]
packList (x:xs) = if x `elem` (head (packList xs))
                  then (x: (head (packList xs)))  : (tail $ packList xs)
                  else [x]: (packList xs)


runLengthEncode :: (Eq a) => [a] -> [(Int, a)]
runLengthEncode xs = 
        map encode $ packList xs
        where encode list = (length list, head list) 