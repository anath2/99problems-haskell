-- Insert at position n

insertAt :: a -> [a] -> Int -> [a] 
insertAt x xs n = (take n xs) ++ [x] ++ (drop n xs) 