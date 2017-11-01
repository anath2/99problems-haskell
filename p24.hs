-- Select distinct random numbers from range 1 upto M

import System.Random

selectFromRange :: Int -> Int -> IO [Int]
selectFromRange _ 0 = return []
selectFromRange n m = selectFromList n [1..m]

selectFromList n xs
    | xs == [] = return []
    | n == 0 = return []
    | otherwise = 
        do 
            r <-  randomRIO (0, (length xs) - 1)
            let remaining = take r xs ++ drop (r + 1) xs
            rest <- selectFromList (n - 1) remaining
            return ((xs !! r) : rest)
