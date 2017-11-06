-- Generate random permutations of a list
import System.Random
import Control.Monad (replicateM)

rnd_select xs n 
    | n < 0     = error "N must be greater than zero."
    | otherwise = replicateM n rand
        where rand = do r <- randomRIO (0, (length xs) - 1)
                        return (xs!!r)


        