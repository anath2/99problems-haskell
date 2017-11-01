-- Return a number of random elements from an array

import System.Random 
import Control.Monad (replicateM)

getRandnElements :: [a] -> Int -> IO [a]
getRandnElements [] _ = return []
getRandnElements xs n
    | n < 0 = return []
    | otherwise = do pos <- replicateM n $ getStdRandom $ randomR (0, (length xs) -1)
                     return [xs !! p | p <- pos]
    
