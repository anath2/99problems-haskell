-- Create a minimum spanning tree from a graph
data Graph a = Graph [a] [(a, a)] deriving (Show, Eq)