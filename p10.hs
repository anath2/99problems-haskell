-- Run length encoding algorithm 
-- Consecutive duplicates of elements are encoded as lists (N E) 
-- where N is the number of duplicates of the element E.

runLengthEncode :: (Eq a) => [a] -> [(Int, a)]

runLengthEncode [] = []
runLengthEncode [(1, a)] = [(1, a)]
runLengthEncode (x:xs) = 
        if x == head runLengthEncode xs
        then (x:(head $ runLengthEncode xs)) : tail runLengthEncode xs
        else x: runLengthEncode xs

-- WIP --