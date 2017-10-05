data EncodedItem a = Tuple (Int, a) | Single a deriving (Show, Eq)

addEncodedItems :: (Eq a) => (encodedItem a) -> (encodedItem b) -> (encodedItem c)
addEncodedItems (Single a) (Single a) = Tuple (2, a)
addEncodedItems (Tuple (num, a)) (Single a) = (Tuple (num + 1, a))

runLengthEncode :: (Eq a) => [a] -> [EncodedItem a]
runLengthEncode [] = []
runLengthEncode [a] = [Single a]
runLengthEncode x:xs
    | x == (decodedItem $ head (runLengthEncode xs)) =
         addEncodedItems $ (Single x) head (runLengthEncode xs)
    | otherwise = (Single a) : runLengthEncode xs
    where 
        decodedItem (EncodedItem a) = a