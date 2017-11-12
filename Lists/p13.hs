-- Encode the list direcly

data EncodedItem a = Tuple Int a | Single a deriving (Show, Eq)

encode' :: (Eq a) => [a] -> [(Int, a)]
encode' = foldr helper []
    where 
        helper x [] = [(1, x)]
        helper x (y@(a, b):ys)
            | x == b = (a + 1, b):ys
            | otherwise = (1, x):y:ys

runLengthEncode :: (Eq a) => [a] -> [EncodedItem a]
runLengthEncode = map helper . encode'
        where
            helper (1, a) = Single a
            helper (num, a) = Tuple num a