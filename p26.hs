-- Generate combination K different objects chosen from a list of N elements

-- ALGO
    -- SELECT K DISTINCT ELEMENTS FROM A LIST
    -- Generate combinations of those K elements

import System.Random
import Control.Monad (replicateM)

rndSelect :: [a] -> Int -> IO [a]
rndSelect [] _ = return []
rndSelect _ 0 = return []
rndSelect xs n = do 
                ind <- randomRIO (0, (length xs) - 1)
                let remaining = take ind xs ++ drop (ind + 1) xs                         
                rest <- rndSelect remaining (n - 1) 
                return ((xs!!ind) : rest)

--[WIP]

-- generateCombinations :: Int -> [a] -> IO [[a]]
-- generateCombinations _ [] = return [[]]
