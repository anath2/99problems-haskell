-- Generate combination K different elements chosen from a list of N elements
import Data.List

getCombos :: Int -> [a] -> [[a]]
getCombos 0 ls = [[]]
getCombos k ls = 
    do
        (x:xs) <- tails ls
        rest <- getCombos (k - 1) xs
        return $ x:rest