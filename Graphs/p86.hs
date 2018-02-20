-- Graph coloration using Welch Powell's algorithm

data Graph a = Graph [a] [(a, a)]
                deriving (Eq, Show)

data Adjacency a = Adj 