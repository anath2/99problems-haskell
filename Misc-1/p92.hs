-- Von Koch's conjecture

import Data.List (sortBy)
import Data.Ord (comparing)

vonKoch :: [(Int, Int)] -> [[(Int, Int)]]
