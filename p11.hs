data EncodedItem a = Tuple (Int, a) | Elem a deriving (Show, Eq, Ord)

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

runLengthEncodePlus :: (Eq a) => [a] -> [EncodedItem a]
runLengthEncodePlus xs = 
        map runLengthEncodePlus $ packList xs
        where runLengthEncodePlus list
                | length list == 1 = Elem (head list)
                | otherwise = Tuple (length list, head list)        

-- Run length encode a single item is the same as 
-- Encoding a list using regular run length encoding