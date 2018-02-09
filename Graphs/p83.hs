-- Construct all spanning trees of a given graph
data Graph a = Graph [a] [(a, a)] deriving (Show, Eq)