 -- Create a list containing integers

 createRange :: Int -> Int -> [Int]

createRange a b
    | a > b = []
    | otherwise = [a] ++ createRange (a + 1) b 

-- TODO: Include negative indices 