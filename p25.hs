-- Generate random permutations of a list

import System.Random
import Control.Monad (replicateM)

rndSelect :: [a] -> IO [a]
rndSelect [] = return []
rndSelect xs = do 
                ind <- randomRIO (0, (length xs) - 1)
                let remaining = take ind xs ++ drop (ind + 1) xs                         
                rest <- rndSelect remaining
                return ((xs!!ind) : rest)



        