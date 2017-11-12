-- Decode the run length algorithm encoded in the previous problem

data EncodedItem a = Tuple (Int, a) | Elem a deriving (Show, Eq, Ord)

decodeRunLength :: [EncodedItem a] -> [a]
decodeRunLength xs = concatMap decodeHelper xs
                        where  
                            decodeHelper (Elem a) = [a]
                            decodeHelper (Tuple (n, a)) = replicate n a