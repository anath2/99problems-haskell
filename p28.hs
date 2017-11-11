-- Arrange them by list
import Data.List
import Data.Ord (comparing)
 
lsort :: [[a]] -> [[a]]
lsort = sortBy (\xs ys -> compare (length xs) (length ys))

lfsort :: [[a]] -> [[a]]
lfsort lists = concat groups
    where groups = lsort $ groupBy equalLength $ lsort lists
          equalLength xs ys = length xs == length ys