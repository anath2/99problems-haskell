-- Generate K regular simple graphs 

data Graph a = Graph [a] [(a, a)]
                        deriving (Show, Eq)
data Adjacency a = Adj [(a, [a])]
                        deriving (Show, Eq)

regular :: Int -> Int -> [Graph Int]
