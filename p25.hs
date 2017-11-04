-- Algorithm

-- Take a random pair of elements and replace them in place
-- Take the sublist removing those two elements and do the same

-- Generate a random permutation of an array
import System.Random

-- replace elements function
replaceElem :: Int -> Int -> [a] -> [a]
replaceElem p q xs = take p interim ++ [ interim !! q ] ++ drop (p + 1) interim 
    where
        interim = take q xs ++ [xs !! p] ++ drop (q + 1) xs
--[WIP]