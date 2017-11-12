 -- Create a list containing integers

createRange :: Int -> Int -> [Int]

createRange a b
    | a == b = [a]
    | a > b = [a] ++ createRange (a - 1) b
    | otherwise = [a] ++ createRange (a + 1) b 
